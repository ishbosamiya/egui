use std::hash::Hash;

use epaint::{
    emath::{Align2, NumExt},
    CircleShape, Color32, Pos2, Rect, Shape, Stroke, TextStyle, Vec2,
};

use crate::{
    layers::ShapeIdx,
    plot::transform::{PlotBounds, ScreenTransform},
    Context, CursorIcon, Id, IdMap, InnerResponse, Layout, PointerButton, Sense, Ui, WidgetText,
};

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

    pub fn get_num_shapes(&self) -> usize {
        match self {
            ParameterShape::Circle => 1,
            ParameterShape::Diamond => 1,
            ParameterShape::Square => 1,
            ParameterShape::Cross => 2,
            ParameterShape::Plus => 2,
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

enum Selected {
    MostRecent,
    NotMostRecent,
}

struct NodeShapeIdx {
    selection_border_idx: Option<ShapeIdx>,
    background_idx: ShapeIdx,
    heading_background_idx: ShapeIdx,
    heading_idx: ShapeIdx,
}

struct NodeDrawData {
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

    fn setup_shapes(
        &mut self,
        ui: &mut Ui,
        selected: Option<Selected>,
        transform: &ScreenTransform,
    ) -> NodeDrawData {
        let node_position = transform.transformed_pos(&self.position);
        let mut ui = ui.child_ui(
            Rect::from_two_pos(node_position, node_position + Vec2::new(self.width, 0.0)),
            *ui.layout(),
        );

        let selection_border_idx = selected.map(|_| ui.painter().add(Shape::Noop));
        let background_idx = ui.painter().add(Shape::Noop);
        let heading_background_idx = ui.painter().add(Shape::Noop);
        let heading_idx = ui.painter().add(Shape::Noop);
        let parameters_draw_data = self
            .parameters
            .iter_mut()
            .map(|parameter| parameter.setup_shapes(&mut ui, self.width))
            .collect();

        NodeDrawData {
            parameters_draw_data,
            node_shape_idx: NodeShapeIdx {
                selection_border_idx,
                background_idx,
                heading_background_idx,
                heading_idx,
            },
        }
    }

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

        if let Some(idx) = node_draw_data.node_shape_idx.selection_border_idx {
            let stroke = match selected.unwrap() {
                Selected::MostRecent => ui.visuals().selection.stroke,
                Selected::NotMostRecent => Stroke::new(
                    ui.visuals().selection.stroke.width,
                    Color32::from_rgb(188, 67, 6),
                ),
            };
            ui.painter().set(
                idx,
                Shape::rect_stroke(background_rect, background_corner_radius, stroke),
            );
        }

        ui.painter().set(
            node_draw_data.node_shape_idx.background_idx,
            Shape::rect_filled(
                background_rect,
                background_corner_radius,
                ui.style().visuals.faint_bg_color,
            ),
        );
    }
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

    pub fn show<R>(
        mut self,
        ui: &mut Ui,
        add_contents: impl FnOnce(&mut Ui) -> R,
    ) -> InnerResponse<R> {
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
            });

        let NodeGraphMemory {
            last_screen_transform,
            selected_nodes,
            node_data,
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

        let mut nodes_draw_data = self.setup_shapes(&mut ui, &selected_nodes, &transform);

        self.draw_shapes(&mut ui, &selected_nodes, &transform, &mut nodes_draw_data);

        let inner = add_contents(&mut ui);

        let memory = NodeGraphMemory {
            last_screen_transform: transform,
            node_data: self
                .nodes
                .iter()
                .map(|node| (node.id, NodeMemory::new(node.position, node.width)))
                .collect(),
            selected_nodes,
        };
        memory.store(ui.ctx(), node_graph_id);

        InnerResponse::new(inner, response)
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
    fn setup_shapes(
        &mut self,
        ui: &mut Ui,
        selected_nodes: &[Id],
        transform: &ScreenTransform,
    ) -> Vec<NodeDrawData> {
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

        let nodes_draw_data: Vec<_> = self
            .nodes
            .iter_mut()
            .map(|node| {
                node.setup_shapes(
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
                    transform,
                )
            })
            .collect();

        nodes_draw_data
    }

    fn draw_shapes(
        &self,
        ui: &mut Ui,
        selected_nodes: &[Id],
        transform: &ScreenTransform,
        nodes_draw_data: &mut [NodeDrawData],
    ) {
        // background is already painted

        debug_assert_eq!(self.nodes.len(), nodes_draw_data.len());

        self.nodes
            .iter()
            .zip(nodes_draw_data.iter_mut())
            .for_each(|(node, node_draw_data)| {
                node.draw_shapes(
                    ui,
                    // TODO: cache whether the node is selected or not
                    // when it is first computed in setup_shapes()
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
    }
}
