use std::hash::Hash;

use epaint::{
    emath::{Align2, NumExt},
    util::FloatOrd,
    CircleShape, Color32, Pos2, Rect, Shape, Stroke, TextStyle, Vec2,
};

use crate::{
    layers::ShapeIdx,
    plot::transform::{PlotBounds, ScreenTransform},
    Context, CursorIcon, Id, IdMap, InnerResponse, Key, Layout, PointerButton, Response, Sense, Ui,
    WidgetText,
};

struct LinkShapeIdx {
    shape_idx: ShapeIdx,
}

struct LinkDrawData {
    shape_idx: LinkShapeIdx,
}

pub struct Link {
    node1: Id,
    parameter1: Id,
    node2: Id,
    parameter2: Id,
}

impl Link {
    pub fn new(node1: Id, parameter1: Id, node2: Id, parameter2: Id) -> Self {
        Self {
            node1,
            parameter1,
            node2,
            parameter2,
        }
    }

    #[must_use]
    fn setup_shapes(ui: &mut Ui) -> LinkDrawData {
        let shape_idx = ui.painter().add(Shape::Noop);

        LinkDrawData {
            shape_idx: LinkShapeIdx { shape_idx },
        }
    }

    #[inline]
    fn get_node<'a>(
        link_node_id: Id,
        nodes: &'a [Node],
        nodes_draw_data: &'a [NodeDrawData],
    ) -> Option<(&'a Node, &'a NodeDrawData)> {
        nodes
            .iter()
            .zip(nodes_draw_data.iter())
            .find(|(node, _node_draw_data)| node.id == link_node_id)
    }

    #[inline]
    fn get_param<'a>(
        link_param_id: Id,
        node: &'a Node,
        node_draw_data: &'a NodeDrawData,
    ) -> Option<(&'a Parameter, &'a ParameterDrawData)> {
        node.parameters
            .iter()
            .zip(node_draw_data.parameters_draw_data.iter())
            .find(|(param, _param_draw_data)| param.id == link_param_id)
    }

    fn draw_shapes(
        &self,
        ui: &mut Ui,
        link_draw_data: &LinkDrawData,
        nodes: &[Node],
        nodes_draw_data: &[NodeDrawData],
    ) {
        let line_stroke = Stroke::new(3.0, Color32::TEMPORARY_COLOR);

        let (node1, node1_draw_data) = Self::get_node(self.node1, nodes, nodes_draw_data)
            .expect("link has node id that doesn't exist");
        let (_param1, param1_draw_data) = Self::get_param(self.parameter1, node1, node1_draw_data)
            .expect("link has parameter id that doesn't exist");

        let (node2, node2_draw_data) = Self::get_node(self.node2, nodes, nodes_draw_data)
            .expect("link has node id that doesn't exist");
        let (_param2, param2_draw_data) = Self::get_param(self.parameter2, node2, node2_draw_data)
            .expect("link has parameter id that doesn't exist");

        let pos1 = param1_draw_data.shape_rect.unwrap().center();
        let pos2 = param2_draw_data.shape_rect.unwrap().center();

        ui.painter().set(
            link_draw_data.shape_idx.shape_idx,
            Shape::line_segment([pos1, pos2], line_stroke),
        );
    }
}

#[derive(PartialEq)]
pub enum ParameterShape {
    Circle,
    Diamond,
    Square,
    Cross,
    Plus,
}

impl Default for ParameterShape {
    fn default() -> Self {
        Self::Circle
    }
}

impl ParameterShape {
    #[must_use]
    pub fn get_shape(
        &self,
        center: Pos2,
        radius: f32,
        fill: Color32,
        stroke: Stroke,
    ) -> Vec<Shape> {
        let tf = |dx: f32, dy: f32| -> Pos2 { center + radius * Vec2::new(dx, dy) };
        let frac_1_sqrt_2 = 1.0 / 2f32.sqrt();

        match self {
            ParameterShape::Circle => vec![Shape::Circle(CircleShape {
                center,
                radius,
                fill,
                stroke,
            })],
            ParameterShape::Diamond => {
                let points = vec![tf(1.0, 0.0), tf(0.0, -1.0), tf(-1.0, 0.0), tf(0.0, 1.0)];
                vec![Shape::convex_polygon(points, fill, stroke)]
            }
            ParameterShape::Square => {
                let points = vec![
                    tf(frac_1_sqrt_2, frac_1_sqrt_2),
                    tf(frac_1_sqrt_2, -frac_1_sqrt_2),
                    tf(-frac_1_sqrt_2, -frac_1_sqrt_2),
                    tf(-frac_1_sqrt_2, frac_1_sqrt_2),
                ];
                vec![Shape::convex_polygon(points, fill, stroke)]
            }
            ParameterShape::Cross => {
                let diagonal1 = [
                    tf(-frac_1_sqrt_2, -frac_1_sqrt_2),
                    tf(frac_1_sqrt_2, frac_1_sqrt_2),
                ];
                let diagonal2 = [
                    tf(frac_1_sqrt_2, -frac_1_sqrt_2),
                    tf(-frac_1_sqrt_2, frac_1_sqrt_2),
                ];
                vec![
                    Shape::line_segment(diagonal1, stroke),
                    Shape::line_segment(diagonal2, stroke),
                ]
            }
            ParameterShape::Plus => {
                let horizontal = [tf(-1.0, 0.0), tf(1.0, 0.0)];
                let vertical = [tf(0.0, -1.0), tf(0.0, 1.0)];
                vec![
                    Shape::line_segment(horizontal, stroke),
                    Shape::line_segment(vertical, stroke),
                ]
            }
        }
    }

    #[must_use]
    pub fn get_num_shapes(&self) -> usize {
        match self {
            ParameterShape::Circle | ParameterShape::Diamond | ParameterShape::Square => 1,
            ParameterShape::Cross | ParameterShape::Plus => 2,
        }
    }
}

/// Various parameter types
pub enum ParameterType {
    Input,
    Output,
    /// Parameter is not linkable
    NoLink,
}

impl Default for ParameterType {
    fn default() -> Self {
        Self::NoLink
    }
}

struct ParameterShapeIdx {
    /// The shape may not exist if the parameter type is
    /// [`ParameterType::NoLink`]
    ///
    /// Some shapes may require more than one shape to paint the final
    /// shape.
    shape_idxs: Option<Vec<ShapeIdx>>,
    // No content shapeidx exists since the content is drawn the first
    // pass itself
}

struct ParameterDrawData {
    shape_rect: Option<Rect>,
    contents_rect: Option<Rect>,

    parameter_shape_idx: ParameterShapeIdx,
}

pub struct Parameter {
    /// Id of the parameters, should be created from the node's id to
    /// ensure the parameters always are
    /// unique. `node_id.with(some_hashable)`
    #[allow(dead_code)]
    id: Id,

    param_type: ParameterType,
    shape: ParameterShape,
    /// Radius of the shape in grid space coordinates
    shape_radius: f32,
    add_contents: Option<Box<dyn FnOnce(&mut Ui)>>,
}

impl Parameter {
    /// Create a new parameters, should always be an indirect call
    /// from the Node that will contain this parameter
    fn new(id: Id) -> Self {
        Self {
            id,

            param_type: ParameterType::default(),
            shape: ParameterShape::default(),
            shape_radius: 0.02,
            add_contents: None,
        }
    }

    pub fn param_type(mut self, param_type: ParameterType) -> Self {
        self.param_type = param_type;
        self
    }

    pub fn shape(mut self, shape: ParameterShape) -> Self {
        self.shape = shape;
        self
    }

    pub fn shape_radius(mut self, shape_radius: f32) -> Self {
        self.shape_radius = shape_radius;
        self
    }

    // TODO: make it show that the user does not have to add the
    // explicit box around the add_contents function/closure
    pub fn show(mut self, add_contents: Box<dyn FnOnce(&mut Ui)>) -> Self {
        self.add_contents = Some(add_contents);
        self
    }

    #[must_use]
    fn setup_shapes(&mut self, ui: &mut Ui, node_width: f32) -> ParameterDrawData {
        let shape_idxs = if !matches!(self.param_type, ParameterType::NoLink) {
            Some(
                (0..self.shape.get_num_shapes())
                    .map(|_| ui.painter().add(Shape::Noop))
                    .collect(),
            )
        } else {
            None
        };

        // the contents have to be drawn in this first pass, it is not
        // possible currently to postone this, once egui supports
        // multi pass or at least 2 pass GUI, it would make sense to
        // rewrite this
        //
        // note: self.add_contents gets consumed here
        let contents_rect = self.add_contents.take().map(|add_contents| {
            let layout = match self.param_type {
                ParameterType::Input | ParameterType::NoLink => Layout::left_to_right(),
                ParameterType::Output => Layout::right_to_left(),
            };
            ui.allocate_ui_with_layout(Vec2::new(node_width, 0.0), layout, add_contents)
                .response
                .rect
        });

        ParameterDrawData {
            shape_rect: None,
            contents_rect,
            parameter_shape_idx: ParameterShapeIdx { shape_idxs },
        }
    }

    fn draw_shapes(
        &self,
        ui: &mut Ui,
        draw_data: &mut ParameterDrawData,
        transform: &ScreenTransform,
    ) {
        if let Some(shape_idxs) = &draw_data.parameter_shape_idx.shape_idxs {
            if let Some(contents_rect) = draw_data.contents_rect {
                let shape_padding = 0.01;
                let shape_padding_screen_space = shape_padding * transform.scale_x();
                let shape_radius_screen_space = self.shape_radius * transform.scale_x();
                let shape_center = match self.param_type {
                    ParameterType::Input => {
                        contents_rect.left_center()
                            + Vec2::new(
                                -(shape_radius_screen_space + shape_padding_screen_space),
                                0.0,
                            )
                    }
                    ParameterType::Output => {
                        contents_rect.right_center()
                            + Vec2::new(shape_radius_screen_space + shape_padding_screen_space, 0.0)
                    }
                    ParameterType::NoLink => unreachable!(),
                };

                let shape_rect = Rect::from_center_size(
                    shape_center,
                    Vec2::new(
                        shape_radius_screen_space * 2.0,
                        shape_radius_screen_space * 2.0,
                    ),
                );

                draw_data.shape_rect = Some(shape_rect);

                let shapes = self.shape.get_shape(
                    shape_center,
                    shape_radius_screen_space,
                    Color32::TEMPORARY_COLOR,
                    match self.shape {
                        ParameterShape::Cross | ParameterShape::Plus => {
                            Stroke::new(shape_radius_screen_space * 0.5, Color32::TEMPORARY_COLOR)
                        }
                        _ => Stroke::none(),
                    },
                );

                debug_assert_eq!(shape_idxs.len(), shapes.len());

                shape_idxs
                    .iter()
                    .zip(shapes.into_iter())
                    .for_each(|(shape_idx, shape)| {
                        ui.painter().set(*shape_idx, shape);
                    });
            } else {
                ui.painter().error(
                    ui.next_widget_position(),
                    "No content provided for the parameter",
                );
            }
        }
    }
}

/// Any element that is interactable must implement this trait.
///
/// The order of operations is as follows. The test for whether the ui
/// can be interacted with with the current state of input/events. If
/// it is possible, then hover indepenent function is called. If this
/// fails to interact, then hover dependent function is called.
trait Interactable<'a> {
    type InteractionResponse;
    type HoverExtraData;
    type NoHoverExtraData;

    /// Checks if any interaction events have taken place. Only if
    /// true, should [`Self::interact_no_hover()`] and/or
    /// [`Self::interact_on_hover()`] be called.
    ///
    /// Note: It is convinent to mention all the various interaction
    /// events by their shortcut even if there is some sort of overlap
    /// of the events between the different interactions.
    #[must_use]
    fn should_interact(ui: &Ui, response: &Response) -> bool;

    /// Interact with UI when interaction is independent of cursor
    /// hovering over the Response. Must return `Some(R)` if any
    /// interaction actually takes place. This is done so that
    /// interaction calls are made for layers below this layer. Must
    /// return `None` if no interaction took place.
    ///
    /// Note: depends on [`Self::should_interact()`]. Only if
    /// [`Self::should_interact()`] returns true will
    /// [`Self::interact_no_hover()`] be called.
    #[must_use]
    fn interact_hover_independent(
        &self,
        ui: &Ui,
        response: &Response,
        extra_data: Self::NoHoverExtraData,
    ) -> Option<Self::InteractionResponse>;

    /// Interact with UI when cursor hovers over the Response. Must
    /// return `Some(R)` if any interaction actually takes place. This
    /// is done so that interaction calls are made for layers below
    /// this layer. Must return `None` if no interaction took place.
    ///
    /// Note: depends on [`Self::should_interact()`]. Only if
    /// [`Self::should_interact()`] returns true will
    /// [`Self::interact_on_hover()`] be called.
    #[must_use]
    fn interact_on_hover(
        &self,
        ui: &Ui,
        response: &Response,
        hover_pos: Pos2,
        extra_data: Self::HoverExtraData,
    ) -> Option<Self::InteractionResponse>;
}

impl<'a> Interactable<'a> for Node {
    type InteractionResponse = NodeInteractionResponse;
    type HoverExtraData = (&'a mut Option<(Id, Id)>, &'a NodeDrawData);
    type NoHoverExtraData = &'a mut Option<(Id, Id)>;

    fn should_interact(ui: &Ui, response: &Response) -> bool {
        // link creation start or end
        response.clicked_by(PointerButton::Primary)
        // cancel link creation
        || ui.input().key_pressed(Key::Escape)
    }

    fn interact_hover_independent(
        &self,
        ui: &Ui,
        _response: &Response,
        link_creation_start: Self::NoHoverExtraData,
    ) -> Option<Self::InteractionResponse> {
        // cancel link creation
        if ui.input().key_pressed(Key::Escape) {
            *link_creation_start = None;
            Some(NodeInteractionResponse::no_response_propagation())
        } else {
            None
        }
    }

    fn interact_on_hover(
        &self,
        _ui: &Ui,
        response: &Response,
        hover_pos: Pos2,
        (link_creation_start, node_draw_data): Self::HoverExtraData,
    ) -> Option<Self::InteractionResponse> {
        let parameter_interaction_dist_sq = 16.0_f32.powi(2);

        let parameter_data = self
            .parameters
            .iter()
            .zip(node_draw_data.parameters_draw_data.iter())
            .filter_map(|(parameter, parameter_draw_data)| {
                parameter_draw_data.shape_rect.and_then(|rect| {
                    let dist_sq = rect.distance_sq_to_pos(hover_pos);
                    if dist_sq < parameter_interaction_dist_sq {
                        Some((dist_sq, parameter, parameter_draw_data))
                    } else {
                        None
                    }
                })
            })
            .min_by_key(|(dist_sq, _, _)| dist_sq.ord());

        // link creation start or end
        if response.clicked_by(PointerButton::Primary) {
            if let Some((_param_dist_sq, parameter, _parameter_draw_data)) = parameter_data {
                // some parameter is interactable

                if let Some((start_node_id, start_parameter_id)) = link_creation_start.take() {
                    // complete link is formed

                    *link_creation_start = None;

                    return Some(NodeInteractionResponse {
                        add_link: Some(Link::new(
                            start_node_id,
                            start_parameter_id,
                            self.id,
                            parameter.id,
                        )),
                    });
                } else {
                    // start of a new link

                    *link_creation_start = Some((self.id, parameter.id));
                    return Some(NodeInteractionResponse::no_response_propagation());
                }
            }
        }
        None
    }
}

struct NodeInteractionResponse {
    add_link: Option<Link>,
}

impl NodeInteractionResponse {
    /// Create new [`Self`] with all responses as [`Option::None`].
    ///
    /// Useful to create this when interaction takes place but no
    /// resulting response is created that must be progagated forward.
    pub fn no_response_propagation() -> Self {
        Self { add_link: None }
    }
}

enum Selected {
    MostRecent,
    NotMostRecent,
}

struct NodeShapeIdx {
    background_border_idx: ShapeIdx,
    background_idx: ShapeIdx,
    heading_background_idx: ShapeIdx,
    heading_idx: ShapeIdx,
}

struct NodeDrawData {
    background_rect: Option<Rect>,
    parameters_draw_data: Vec<ParameterDrawData>,

    node_shape_idx: NodeShapeIdx,
}

pub struct Node {
    /// Id of the node, used to create unique Id(s) for the inputs and
    /// outputs
    id: Id,

    name: WidgetText,
    parameters: Vec<Parameter>,

    /// Position of top left of the node in the node graph, it not
    /// strictly the top left, it is roughly the top left of the node.
    position: Pos2,
    /// Width of the node
    ///
    /// TODO: width is currently in screen space, need to convert it
    /// to the node graph space
    width: f32,
}

impl Node {
    pub fn new(name: impl Into<WidgetText>, position: Pos2) -> Self {
        let name = name.into();
        Self {
            id: Id::new(name.text()),
            name,
            parameters: Vec::new(),
            position,
            width: 100.0,
        }
    }

    pub fn id(mut self, id_source: impl Hash) -> Self {
        self.id = Id::new(id_source);
        self
    }

    pub fn parameter(
        mut self,
        param_id: impl Hash,
        build_parameter: impl FnOnce(Parameter) -> Parameter,
    ) -> Self {
        let parameter = Parameter::new(self.id.with(param_id));
        self.parameters.push(build_parameter(parameter));
        self
    }

    pub fn width(mut self, width: f32) -> Self {
        self.width = width;
        self
    }

    #[must_use]
    fn setup_shapes(&mut self, ui: &mut Ui, transform: &ScreenTransform) -> NodeDrawData {
        let node_position = transform.transformed_pos(&self.position);
        let mut ui = ui.child_ui(
            Rect::from_two_pos(node_position, node_position + Vec2::new(self.width, 0.0)),
            *ui.layout(),
        );

        let background_border_idx = ui.painter().add(Shape::Noop);
        let background_idx = ui.painter().add(Shape::Noop);
        let heading_background_idx = ui.painter().add(Shape::Noop);
        let heading_idx = ui.painter().add(Shape::Noop);
        let parameters_draw_data = self
            .parameters
            .iter_mut()
            .map(|parameter| parameter.setup_shapes(&mut ui, self.width))
            .collect();

        NodeDrawData {
            background_rect: None,
            parameters_draw_data,
            node_shape_idx: NodeShapeIdx {
                background_border_idx,
                background_idx,
                heading_background_idx,
                heading_idx,
            },
        }
    }

    #[must_use]
    fn calculate_total_parameters_rect(&self, node_draw_data: &NodeDrawData) -> Rect {
        self.parameters
            .iter()
            .zip(node_draw_data.parameters_draw_data.iter())
            .fold(Rect::NOTHING, |acc, (parameter, draw_data)| {
                let acc = if let Some(rect) = draw_data.shape_rect {
                    // hack: to ensure the shape is on the edge of the
                    // background, consider only the appropriate side
                    // of the rect
                    let rect = match parameter.param_type {
                        ParameterType::Input => {
                            Rect::from_two_pos(rect.center_top(), rect.right_bottom())
                        }
                        ParameterType::Output => {
                            Rect::from_two_pos(rect.left_top(), rect.center_bottom())
                        }
                        ParameterType::NoLink => unreachable!(),
                    };
                    acc.union(rect)
                } else {
                    acc
                };
                if let Some(rect) = draw_data.contents_rect {
                    acc.union(rect)
                } else {
                    acc
                }
            })
    }

    fn draw_shapes(
        &self,
        ui: &mut Ui,
        selected: Option<Selected>,
        node_draw_data: &mut NodeDrawData,
        transform: &ScreenTransform,
    ) {
        let background_stroke_color = ui.visuals().code_bg_color;
        let background_color = ui.visuals().extreme_bg_color;
        let background_corner_radius = 2.0;
        let heading_color = ui.style().visuals.text_color();
        let heading_background_corner_radius = 2.0_f32.min(background_corner_radius);
        let heading_background_color = Color32::DARK_BLUE;

        // parameter shapes
        {
            self.parameters
                .iter()
                .zip(node_draw_data.parameters_draw_data.iter_mut())
                .for_each(|(parameter, parameter_draw_data)| {
                    parameter.draw_shapes(ui, parameter_draw_data, transform);
                });
        }

        let total_parameters_rect = self.calculate_total_parameters_rect(node_draw_data);

        // handle heading
        let heading_rect = {
            let heading_pos_center_bottom = total_parameters_rect.center_top();
            let heading_galley = ui.fonts().layout(
                self.name.text().to_string(),
                TextStyle::Body,
                heading_color,
                total_parameters_rect.width(),
            );
            let heading_rect = Align2::LEFT_BOTTOM.anchor_rect(Rect::from_two_pos(
                heading_pos_center_bottom + Vec2::new(-heading_galley.size().x * 0.5, 0.0),
                heading_pos_center_bottom
                    + Vec2::new(heading_galley.size().x * 0.5, heading_galley.size().y),
            ));

            let heading_shape = Shape::galley(heading_rect.min, heading_galley);
            ui.painter()
                .set(node_draw_data.node_shape_idx.heading_idx, heading_shape);

            heading_rect
        };

        let background_rect = total_parameters_rect.union(heading_rect);

        // heading background
        {
            let heading_background_rect = Rect::from_center_size(
                heading_rect.center(),
                Vec2::new(background_rect.width(), heading_rect.height()),
            );

            ui.painter().set(
                node_draw_data.node_shape_idx.heading_background_idx,
                Shape::rect_filled(
                    heading_background_rect,
                    heading_background_corner_radius,
                    heading_background_color,
                ),
            );
        }

        let stroke = match selected {
            Some(selected) => match selected {
                Selected::MostRecent => ui.visuals().selection.stroke,
                Selected::NotMostRecent => Stroke::new(
                    ui.visuals().selection.stroke.width,
                    Color32::from_rgb(188, 67, 6),
                ),
            },
            None => Stroke::new(ui.visuals().selection.stroke.width, background_stroke_color),
        };
        ui.painter().set(
            node_draw_data.node_shape_idx.background_border_idx,
            Shape::rect_stroke(background_rect, background_corner_radius, stroke),
        );

        ui.painter().set(
            node_draw_data.node_shape_idx.background_idx,
            Shape::rect_filled(background_rect, background_corner_radius, background_color),
        );

        node_draw_data.background_rect = Some(background_rect);
    }
}

struct InteractionResponse {
    /// Currently selected nodes
    selected_nodes: Vec<Id>,
    /// New link creation. Is [`Option::Some`] if a new link creation
    /// is in progress with a tuple of NodeId and ParameterId
    link_creation_start: Option<(Id, Id)>,
    /// Is [`Option::Some`] if a new link is created along with the
    /// link.
    add_link: Option<Link>,
    /// Is [`Option::Some`] if a link is deleted along with the index
    /// of the deleted link in the links list.
    delete_link: Option<usize>,
}

/// Information about the node that has to persist between frames.
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone)]
struct NodeMemory {
    position: Pos2,
    width: f32,
}

impl NodeMemory {
    pub fn new(position: Pos2, width: f32) -> Self {
        Self { position, width }
    }
}

/// Information about the node graph that has to persist between frames.
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone)]
struct NodeGraphMemory {
    last_screen_transform: ScreenTransform,
    node_data: IdMap<NodeMemory>,
    /// List of nodes that are currently selected, with selection
    /// order going from left to right as least recent to most recent.
    selected_nodes: Vec<Id>,
    /// If [`Option::Some`], a new link is currently being created
    /// which contains the source's node id and parameter id.
    link_creation_start: Option<(Id, Id)>,
}

impl NodeGraphMemory {
    pub fn load(ctx: &Context, id: Id) -> Option<Self> {
        ctx.memory().data.get_persisted(id)
    }

    pub fn store(self, ctx: &Context, id: Id) {
        ctx.memory().data.insert_persisted(id, self);
    }
}

#[must_use = "You should put this widget in an ui with `ui.add(widget);`"]
pub struct NodeGraph {
    id_source: Id,

    /// Show axis to provide sense of scale. Default true.
    show_axis: bool,

    min_size: Vec2,
    /// Width of the node graph
    width: Option<f32>,
    /// Height of the node graph
    height: Option<f32>,
    /// Width / height ratio of the node graph region
    view_aspect: Option<f32>,

    nodes: Vec<Node>,
}

pub struct NodeGraphResponse<R> {
    pub add_link: Option<Link>,
    pub delete_link: Option<usize>,

    pub inner_response: InnerResponse<R>,
}

impl NodeGraph {
    pub fn new(id_source: impl std::hash::Hash) -> Self {
        Self {
            id_source: Id::new(id_source),

            show_axis: true,

            min_size: Vec2::splat(64.0),
            width: None,
            height: None,
            view_aspect: None,

            nodes: Vec::new(),
        }
    }

    pub fn show_axis(mut self, show_axis: bool) -> Self {
        self.show_axis = show_axis;
        self
    }

    /// width / height ratio of the node graph region. By default no
    /// fixed aspect ratio is set (and width/height will fill the ui
    /// it is in).
    pub fn view_aspect(mut self, view_aspect: f32) -> Self {
        self.view_aspect = Some(view_aspect);
        self
    }

    /// Width of node graph. By default a node graph will fill the ui
    /// it is in. If you set [`Self::view_aspect`], the width can be
    /// calculated from the height.
    pub fn width(mut self, width: f32) -> Self {
        self.min_size.x = width;
        self.width = Some(width);
        self
    }

    /// Height of node graph. By default a node graph will fill the ui
    /// it is in.  If you set [`Self::view_aspect`], the height can be
    /// calculated from the width.
    pub fn height(mut self, height: f32) -> Self {
        self.min_size.y = height;
        self.height = Some(height);
        self
    }

    /// Minimum size of the node graph view.
    pub fn min_size(mut self, min_size: Vec2) -> Self {
        self.min_size = min_size;
        self
    }

    pub fn node(mut self, node: Node) -> Self {
        self.nodes.push(node);
        self
    }

    #[must_use]
    pub fn show<R>(
        mut self,
        ui: &mut Ui,
        links: &[Link],
        add_contents: impl FnOnce(&mut Ui) -> R,
    ) -> NodeGraphResponse<R> {
        let center_x_axis = false;
        let center_y_axis = false;

        // Determine the size of the node graph in the UI
        let size = {
            let width = self
                .width
                .unwrap_or_else(|| {
                    if let (Some(height), Some(aspect)) = (self.height, self.view_aspect) {
                        height * aspect
                    } else {
                        ui.available_size_before_wrap().x
                    }
                })
                .at_least(self.min_size.x);

            let height = self
                .height
                .unwrap_or_else(|| {
                    if let Some(aspect) = self.view_aspect {
                        width / aspect
                    } else {
                        ui.available_size_before_wrap().y
                    }
                })
                .at_least(self.min_size.y);
            epaint::vec2(width, height)
        };

        // Allocate the space.
        let (rect, mut response) = ui.allocate_exact_size(size, Sense::click_and_drag());

        // Load or initialize the memory.
        let node_graph_id = ui.make_persistent_id(self.id_source);
        let memory =
            NodeGraphMemory::load(ui.ctx(), node_graph_id).unwrap_or_else(|| NodeGraphMemory {
                last_screen_transform: {
                    let mut transform = ScreenTransform::new(
                        rect,
                        PlotBounds::NOTHING,
                        center_x_axis,
                        center_y_axis,
                    );
                    // force the aspect ratio to be 1
                    transform.set_aspect(1.0);
                    transform
                },
                node_data: self
                    .nodes
                    .iter()
                    .map(|node| (node.id, NodeMemory::new(node.position, node.width)))
                    .collect(),
                selected_nodes: Vec::new(),
                link_creation_start: None,
            });

        let NodeGraphMemory {
            last_screen_transform,
            selected_nodes,
            node_data,
            link_creation_start,
        } = memory;

        // reorder nodes based selection history
        self.nodes.sort_by_key(|node| {
            selected_nodes
                .iter()
                .enumerate()
                .find_map(|(index, id)| {
                    if *id == node.id {
                        Some(selected_nodes.len() + index + 1)
                    } else {
                        None
                    }
                })
                .unwrap_or(selected_nodes.len())
        });

        // force nodes to their respective positions
        self.nodes.iter_mut().for_each(|node| {
            if let Some(data) = node_data.get(&node.id) {
                node.position = data.position;
            }
        });

        let transform = {
            let mut transform = ScreenTransform::new(
                rect,
                *last_screen_transform.bounds(),
                center_x_axis,
                center_y_axis,
            );

            transform.restore_aspect_ratio(&last_screen_transform);

            // Dragging
            if response.dragged_by(PointerButton::Middle)
                || (ui.input().modifiers.alt_only() && response.dragged_by(PointerButton::Primary))
            {
                response = response.on_hover_cursor(CursorIcon::Grabbing);
                transform.translate_bounds(-response.drag_delta());
            }

            // Zooming
            if let Some(hover_pos) = response.hover_pos() {
                let zoom_factor = Vec2::splat(ui.input().zoom_delta());
                if zoom_factor != Vec2::splat(1.0) {
                    transform.zoom(zoom_factor, hover_pos);
                }

                let scroll_delta = ui.input().scroll_delta;
                if scroll_delta != Vec2::ZERO {
                    transform.translate_bounds(-scroll_delta);
                }
            }

            transform
        };

        if !selected_nodes.is_empty()
            && response.dragged_by(PointerButton::Primary)
            && ui.input().modifiers.is_none()
        {
            for selected_node in selected_nodes.iter() {
                if let Some(node) = self.nodes.iter_mut().find(|node| node.id == *selected_node) {
                    node.position += Vec2::new(
                        response.drag_delta().x * transform.dvalue_dpos()[0] as f32,
                        response.drag_delta().y * transform.dvalue_dpos()[1] as f32,
                    );
                }
            }
        }

        let mut ui = ui.child_ui(rect, *ui.layout());

        let (links_node_data, mut nodes_draw_data) = self.setup_shapes(&mut ui, links, &transform);

        self.draw_shapes(
            &mut ui,
            &selected_nodes,
            &transform,
            links,
            &links_node_data,
            &mut nodes_draw_data,
        );

        let InteractionResponse {
            selected_nodes,
            link_creation_start,
            add_link,
            delete_link,
        } = self.interact(
            &ui,
            &response,
            selected_nodes,
            link_creation_start,
            &nodes_draw_data,
        );

        self.draw_interaction_shapes(&mut ui, &link_creation_start, &nodes_draw_data);

        let inner = add_contents(&mut ui);

        let memory = NodeGraphMemory {
            last_screen_transform: transform,
            node_data: self
                .nodes
                .iter()
                .map(|node| (node.id, NodeMemory::new(node.position, node.width)))
                .collect(),
            selected_nodes,
            link_creation_start,
        };
        memory.store(ui.ctx(), node_graph_id);

        NodeGraphResponse {
            add_link,
            delete_link,
            inner_response: InnerResponse::new(inner, response),
        }
    }

    /// Setup the layering of the UI elements. Elements whose position
    /// or size is not dependent on anything else might be drawn
    /// directly, no requirement to create placeholder shapes. For any
    /// elements that depend on some other elements to be drawn first,
    /// must create placeholder shapes, so layering is defined. The
    /// next call to [`Self::draw_shapes()`] draws the rest of the
    /// shapes.
    ///
    /// TODO: Elements that can be interacted with must create the
    /// interactable element at this stage.
    #[must_use]
    fn setup_shapes(
        &mut self,
        ui: &mut Ui,
        links: &[Link],
        transform: &ScreenTransform,
    ) -> (Vec<LinkDrawData>, Vec<NodeDrawData>) {
        // TODO: need to figure out if ui.child_ui() must be done or
        // not each of the different elements.

        ui.set_clip_rect(*transform.frame());

        // background grid is first
        if self.show_axis {
            let mut shapes = Vec::new();
            for d in 0..2 {
                crate::paint_axis(ui, d, transform, true, &mut shapes);
            }

            // grid does not depend on other elements, no need to
            // create place older shapes that will be updated later
            ui.painter().extend(shapes);
        }

        // links
        let links_draw_data = links.iter().map(|_link| Link::setup_shapes(ui)).collect();

        // nodes
        let nodes_draw_data = self
            .nodes
            .iter_mut()
            .map(|node| node.setup_shapes(ui, transform))
            .collect();

        (links_draw_data, nodes_draw_data)
    }

    /// Draws the final shapes and updates any missing draw data.
    fn draw_shapes(
        &self,
        ui: &mut Ui,
        selected_nodes: &[Id],
        transform: &ScreenTransform,
        links: &[Link],
        links_draw_data: &[LinkDrawData],
        nodes_draw_data: &mut [NodeDrawData],
    ) {
        // order in which the elements are drawn now does not matter
        // since the layering part is already done in the first pass
        // aka setup_shapes()

        // background is already painted

        // nodes must be drawn before links since the parameter shape
        // rects need to be setup in the NodeDrawData
        debug_assert_eq!(self.nodes.len(), nodes_draw_data.len());
        self.nodes
            .iter()
            .zip(nodes_draw_data.iter_mut())
            .for_each(|(node, node_draw_data)| {
                node.draw_shapes(
                    ui,
                    selected_nodes
                        .iter()
                        .find(|selected_node| **selected_node == node.id)
                        .map(|selected_node| {
                            if selected_node == selected_nodes.last().unwrap() {
                                Selected::MostRecent
                            } else {
                                Selected::NotMostRecent
                            }
                        }),
                    node_draw_data,
                    transform,
                );
            });

        debug_assert_eq!(links.len(), links_draw_data.len());
        links
            .iter()
            .zip(links_draw_data.iter())
            .for_each(|(link, link_draw_data)| {
                link.draw_shapes(ui, link_draw_data, &self.nodes, nodes_draw_data);
            });
    }

    /// Draw the shapes necessary to indicate some interaction. Unless
    /// some interaction is expected and setup by
    /// [`Self::setup_shapes`], this pass will always be painted over
    /// all other shapes.
    fn draw_interaction_shapes(
        &self,
        ui: &mut Ui,
        link_creation_start: &Option<(Id, Id)>,
        nodes_draw_data: &[NodeDrawData],
    ) {
        // TODO: make this consistent with the Link line stroke, this
        // will be a problem when Link gets more customization.
        let line_stroke = Stroke::new(3.0, Color32::TEMPORARY_COLOR);

        if let Some((start_node_id, start_parameter_id)) = link_creation_start {
            let (node, node_draw_data) =
                Link::get_node(*start_node_id, &self.nodes, nodes_draw_data)
                    .expect("link creation start has a node id that does not exist");
            let (_parameter, parameter_draw_data) =
                Link::get_param(*start_parameter_id, node, node_draw_data)
                    .expect("link creation start has a parameter id that does not exist");

            let pos1 = parameter_draw_data.shape_rect.unwrap().center();
            if let Some(pos2) = ui.input().pointer.hover_pos() {
                ui.painter().line_segment([pos1, pos2], line_stroke);
            }
        }
    }

    /// Should node graph specific interaction take place?
    ///
    /// This does not include the nodes or links or any sub component
    /// of the node graph. Applies to only events that are global but
    /// specific to the node graph such as selection of nodes or
    /// selection of links, etc.
    fn should_interact(ui: &Ui, response: &Response) -> bool {
        // selection or deselection of nodes and/or links
        (response.clicked_by(PointerButton::Primary) && ui.input().modifiers.is_none())
        // add or remove nodes and/or links from selected list
        || (response.clicked_by(PointerButton::Primary) && ui.input().modifiers.shift_only())
    }

    #[must_use]
    fn interact(
        &self,
        ui: &Ui,
        response: &Response,
        mut selected_nodes: Vec<Id>,
        mut link_creation_start: Option<(Id, Id)>,
        nodes_draw_data: &[NodeDrawData],
    ) -> InteractionResponse {
        let node_interaction_dist_sq = 16.0_f32.powi(2);

        let mut add_link = None;

        // non hover dependent interactions
        {
            // TODO: some of the interactions in the node should not
            // be part of the node interaction, it should be a
            // separate thing. eg: link creation, it should be it's
            // own thing

            let interaction_res = self.nodes.iter().try_for_each(|node| {
                // this can get confusing, if interaction takes place,
                // then need to end the for loop Err(contents of the
                // interaction response) is returned. If no Err is
                // returned by the try_for_each(), no interaction took
                // place
                if let Some(response) =
                    node.interact_hover_independent(ui, response, &mut link_creation_start)
                {
                    Err(response)
                } else {
                    Ok(())
                }
            });

            // this is the confusing part, an Err as the result means
            // that interaction has taken place
            if let Err(response) = interaction_res {
                return InteractionResponse {
                    selected_nodes,
                    link_creation_start,
                    add_link: response.add_link,
                    delete_link: None,
                };
            }
        }

        if let Some(hover_pos) = response.hover_pos() {
            // node graph level interaction also requires node data,
            // so compute at the same time
            let node_data = (Self::should_interact(ui, response)
                || Node::should_interact(ui, response))
            .then(|| ())
            .and_then(|_| {
                self.nodes
                    .iter()
                    .zip(nodes_draw_data.iter())
                    .filter_map(|(node, node_draw_data)| {
                        let dist_sq = node_draw_data
                            .background_rect
                            .unwrap()
                            .distance_sq_to_pos(hover_pos);
                        if dist_sq < node_interaction_dist_sq {
                            Some((dist_sq, node, node_draw_data))
                        } else {
                            None
                        }
                    })
                    .min_by_key(|(dist_sq, _, _)| dist_sq.ord())
            });

            if let Some((_node_dist_sq, node, node_draw_data)) = node_data {
                // To ensure order of interactions is done correctly,
                // it should be the top most layer first, followed in
                // order of the layering of the UI elements
                #[allow(clippy::collapsible_else_if)]
                // node specific interaction
                if let Some(interaction_response) = node.interact_on_hover(
                    ui,
                    response,
                    hover_pos,
                    (&mut link_creation_start, node_draw_data),
                ) {
                    add_link = interaction_response.add_link;
                }
                // node graph specific interaction
                else {
                    if response.clicked_by(PointerButton::Primary) && ui.input().modifiers.is_none()
                    {
                        // select only one node
                        selected_nodes.clear();
                        selected_nodes.push(node.id);
                    } else if response.clicked_by(PointerButton::Primary)
                        && ui.input().modifiers.shift_only()
                    {
                        // add or remove one node from selected node
                        // list
                        let selected_node_index = selected_nodes
                            .iter()
                            .enumerate()
                            .find(|(_index, selected_node)| **selected_node == node.id)
                            .map(|(index, _)| index);

                        if let Some(selected_node_index) = selected_node_index {
                            // if most recently selected node
                            if selected_node_index == selected_nodes.len() - 1 {
                                // remove node
                                selected_nodes.remove(selected_node_index);
                            } else {
                                // make this node as most recently selected
                                let node_id = selected_nodes.remove(selected_node_index);
                                selected_nodes.push(node_id);
                            }
                        } else {
                            // add node to selected nodes list
                            selected_nodes.push(node.id);
                        }
                    }
                }
            } else {
                // deselect all the nodes
                if response.clicked_by(PointerButton::Primary) && ui.input().modifiers.is_none() {
                    selected_nodes.clear();
                }
            }
        }

        InteractionResponse {
            selected_nodes,
            link_creation_start,
            add_link,
            delete_link: None,
        }
    }
}
