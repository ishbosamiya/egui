use std::hash::Hash;

use epaint::{
    emath::{Align2, NumExt},
    util::FloatOrd,
    CircleShape, Color32, Pos2, Rect, Shape, Stroke, TextStyle, Vec2,
};

use crate::{
    plot::transform::{PlotBounds, ScreenTransform},
    Context, CursorIcon, Id, IdMap, InnerResponse, Layout, PointerButton, Response, Sense, Ui,
    WidgetText,
};

enum InteractionType {
    Node,
    NodeEdge,
    ParameterLinker(Id),
}

impl InteractionType {
    /// Get layer order of the interaction type, 0 is lowest in the
    /// layer stack
    #[inline]
    pub fn get_layer_order(&self) -> usize {
        match self {
            InteractionType::Node => 0,
            InteractionType::NodeEdge => 1,
            InteractionType::ParameterLinker(_) => 2,
        }
    }
}

struct Interactable {
    /// Id of the [`Node`] for which interaction may take place
    node_id: Id,
    /// Region of space covered by the interactable object
    rect: Rect,
    /// Interaction type of the interactable
    interaction_type: InteractionType,
}

impl Interactable {
    pub fn new(node_id: Id, rect: Rect, interaction_type: InteractionType) -> Self {
        Self {
            node_id,
            rect,
            interaction_type,
        }
    }

    /// Translate the interactable region with the given delta.
    pub fn translate(&mut self, delta: Vec2) {
        self.rect = self.rect.translate(delta);
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
}

pub struct Parameter {
    name: WidgetText,
    shape: ParameterShape,
}

impl Parameter {
    pub fn new(name: impl Into<WidgetText>, shape: ParameterShape) -> Self {
        Self {
            name: name.into(),
            shape,
        }
    }

    /// Create the UI but do not paint it, add the shapes the must be
    /// painted to shapes
    #[allow(clippy::too_many_arguments)]
    fn ui(
        &self,
        node_id: Id,
        parameter_id: Id,
        ui: &Ui,
        pos: Pos2,
        radius: f32,
        max_width: f32,
        is_input: bool,
    ) -> (Vec<Shape>, Vec<Interactable>, Rect) {
        // TODO: need to scale the parameter based on the transform

        let text_style = TextStyle::Body;
        let galley = ui.fonts().layout(
            self.name.text().to_string(),
            text_style,
            ui.style().visuals.text_color(),
            // hack: only half of the shape is
            // considered, see shape_rect hack for more information
            max_width - radius,
        );

        let align = if is_input {
            Align2::LEFT_TOP
        } else {
            Align2::RIGHT_TOP
        };

        let pos = if is_input {
            pos
        } else {
            pos + Vec2::new(max_width, 0.0)
        };

        let text_pos = if is_input {
            pos + Vec2::new(2.0 * radius, 0.0)
        } else {
            pos - Vec2::new(2.0 * radius, 0.0)
        };
        let text_rect = align.anchor_rect(Rect::from_min_size(text_pos, galley.size()));

        let center = pos + Vec2::new(0.0, text_rect.height() * 0.5);

        // hack: need the shape to be on the background rect, so shape
        // rect does not actually cover the whole shape, only half of
        // the shape.
        let hack_shape_rect = Rect::from_center_size(
            center + Vec2::new(0.5 * radius, 0.0),
            Vec2::new(radius, 2.0 * radius),
        );

        let shape_rect = Rect::from_center_size(center, Vec2::new(2.0 * radius, 2.0 * radius));

        let mut shapes = self.shape.get_shape(
            center,
            radius,
            Color32::TEMPORARY_COLOR,
            match self.shape {
                ParameterShape::Cross | ParameterShape::Plus => {
                    Stroke::new(radius * 0.5, Color32::TEMPORARY_COLOR)
                }
                _ => Stroke::none(),
            },
        );

        let final_rect = hack_shape_rect.union(text_rect);

        shapes.push(Shape::galley(text_rect.min, galley));

        (
            shapes,
            vec![Interactable::new(
                node_id,
                shape_rect,
                InteractionType::ParameterLinker(parameter_id),
            )],
            final_rect,
        )
    }
}

enum Selected {
    MostRecent,
    NotMostRecent,
}

pub struct Node {
    /// Id of the node, used to create unique Id(s) for the inputs and
    /// outputs
    id: Id,

    name: WidgetText,
    inputs: Vec<(Id, Parameter)>,
    outputs: Vec<(Id, Parameter)>,

    /// Position of top left of the node in the node graph
    position: Pos2,
    /// Width of the node
    width: f32,
}

impl Node {
    pub fn new(name: impl Into<WidgetText>, position: Pos2) -> Self {
        let name = name.into();
        Self {
            id: Id::new(name.text()),
            name,
            inputs: Vec::new(),
            outputs: Vec::new(),
            position,
            width: 100.0,
        }
    }

    pub fn id(mut self, id_source: impl Hash) -> Self {
        self.id = Id::new(id_source);
        self
    }

    pub fn input(mut self, parameter: Parameter) -> Self {
        self.inputs
            .push((self.id.with(parameter.name.text()), parameter));
        self
    }

    pub fn inputs(self, parameters: Vec<Parameter>) -> Self {
        parameters
            .into_iter()
            .fold(self, |acc, parameter| acc.input(parameter))
    }

    pub fn output(mut self, parameter: Parameter) -> Self {
        self.outputs
            .push((self.id.with(parameter.name.text()), parameter));
        self
    }

    pub fn outputs(self, parameters: Vec<Parameter>) -> Self {
        parameters
            .into_iter()
            .fold(self, |acc, parameter| acc.output(parameter))
    }

    pub fn width(mut self, width: f32) -> Self {
        self.width = width;
        self
    }

    /// Draw the ui and return the final footprint of the ui for
    /// further interaction tests
    fn ui(
        &self,
        selected: Option<Selected>,
        ui: &mut Ui,
        transform: &ScreenTransform,
    ) -> Vec<Interactable> {
        ui.set_clip_rect(*transform.frame());

        let color = ui.style().visuals.text_color();
        let heading_galley =
            ui.fonts()
                .layout_no_wrap(self.name.text().to_string(), TextStyle::Heading, color);

        let initial_pos = transform.transformed_pos(&self.position);
        let pos = initial_pos + Vec2::new(0.0, heading_galley.size().y);
        // TODO: make the radius scale with the transform using `let
        // radius = 0.02 * transform.dpos_dvalue_x() as f32;`, for
        // now, since the text does not scale with the transform, not
        // scaling the other shapes either
        //
        // hack: Assuming that only TextStyle::Body is used for the
        // parameters.
        let radius = ui.fonts().row_height(TextStyle::Body) * 0.3;

        // first the outputs, it is less common to have the number of
        // inputs to be larger than the number of outputs, so having
        // the lesser number parameters first makes it nicer.
        //
        // TODO: need to make this generic, pick whichever has lesser
        // number of parameters

        let mut outputs_param_size_x = None;
        let mut outputs_param_size_y = 0.0;
        let (output_shapes, output_interactables): (Vec<_>, Vec<_>) = self
            .outputs
            .iter()
            .map(|(parameter_id, output)| {
                let (new_shapes, interactables, rect) = output.ui(
                    self.id,
                    *parameter_id,
                    ui,
                    pos + Vec2::new(0.0, outputs_param_size_y),
                    radius,
                    self.width,
                    false,
                );

                outputs_param_size_x = if let Some(outputs_param_size_x) = outputs_param_size_x {
                    Some(rect.width().max(outputs_param_size_x))
                } else {
                    Some(rect.width())
                };
                outputs_param_size_y += rect.height();

                (new_shapes, interactables)
            })
            .unzip();
        let mut output_shapes: Vec<Shape> = output_shapes.into_iter().flatten().collect();
        let mut output_interactables: Vec<Interactable> =
            output_interactables.into_iter().flatten().collect();

        let outputs_param_size =
            Vec2::new(outputs_param_size_x.unwrap_or(0.0), outputs_param_size_y);

        let pos = pos + Vec2::new(0.0, outputs_param_size.y);
        let mut inputs_param_size_x = None;
        let mut inputs_param_size_y = 0.0;
        let (input_shapes, input_interactables): (Vec<_>, Vec<_>) = self
            .inputs
            .iter()
            .map(|(parameter_id, input)| {
                let (new_shapes, interactables, rect) = input.ui(
                    self.id,
                    *parameter_id,
                    ui,
                    pos + Vec2::new(0.0, inputs_param_size_y),
                    radius,
                    self.width,
                    true,
                );

                inputs_param_size_x = if let Some(inputs_param_size_x) = inputs_param_size_x {
                    Some(rect.width().max(inputs_param_size_x))
                } else {
                    Some(rect.width())
                };
                inputs_param_size_y += rect.height();

                (new_shapes, interactables)
            })
            .unzip();
        let input_shapes: Vec<Shape> = input_shapes.into_iter().flatten().collect();
        let input_interactables: Vec<Interactable> =
            input_interactables.into_iter().flatten().collect();
        let inputs_param_size = Vec2::new(inputs_param_size_x.unwrap_or(0.0), inputs_param_size_y);

        let background_width = inputs_param_size
            .x
            .max(outputs_param_size.x)
            .max(self.width);

        let mut shapes = Vec::new();

        let heading_rect = Align2::CENTER_TOP.anchor_rect(Rect::from_min_size(
            initial_pos + Vec2::new(background_width * 0.5, 0.0),
            heading_galley.size(),
        ));
        shapes.push(Shape::galley(heading_rect.min, heading_galley));

        // must translate all the output elements
        if background_width > self.width {
            for shape in output_shapes.iter_mut() {
                shape.translate(Vec2::new(background_width - self.width, 0.0));
            }
            for interactable in output_interactables.iter_mut() {
                interactable.translate(Vec2::new(background_width - self.width, 0.0));
            }
        }

        let background_rect = Rect::from_min_size(
            initial_pos,
            Vec2::new(
                background_width,
                inputs_param_size.y + outputs_param_size.y + heading_rect.height(),
            ),
        );

        if let Some(selected) = selected {
            let stroke = match selected {
                Selected::MostRecent => ui.visuals().selection.stroke,
                Selected::NotMostRecent => Stroke::new(
                    ui.visuals().selection.stroke.width,
                    Color32::from_rgb(188, 67, 6),
                ),
            };
            ui.painter().rect_stroke(background_rect, 2.0, stroke);
        }
        ui.painter()
            .rect_filled(background_rect, 2.0, ui.style().visuals.faint_bg_color);

        let ui = ui.child_ui(background_rect, Layout::default());

        ui.painter().sub_region(*transform.frame()).extend(shapes);
        ui.painter()
            .sub_region(*transform.frame())
            .extend(output_shapes);
        ui.painter()
            .sub_region(*transform.frame())
            .extend(input_shapes);

        let mut interactables = vec![
            Interactable::new(self.id, background_rect, InteractionType::Node),
            Interactable::new(
                self.id,
                Rect::from_min_max(background_rect.left_top(), background_rect.left_bottom()),
                InteractionType::NodeEdge,
            ),
            Interactable::new(
                self.id,
                Rect::from_min_max(background_rect.right_top(), background_rect.right_bottom()),
                InteractionType::NodeEdge,
            ),
        ];

        let mut output_interactables = output_interactables;
        interactables.append(&mut output_interactables);
        let mut input_interactables = input_interactables;
        interactables.append(&mut input_interactables);

        interactables
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
    auto_bounds: bool,
    min_auto_bounds: PlotBounds,
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

    min_auto_bounds: PlotBounds,

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

            min_auto_bounds: PlotBounds::NOTHING,

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

    /// Draw the GUI and provide the list of interactables.
    fn ui(
        &self,
        selected_nodes: &[Id],
        ui: &mut Ui,
        transform: &ScreenTransform,
    ) -> Vec<Interactable> {
        let mut shapes = Vec::new();

        if self.show_axis {
            for d in 0..2 {
                crate::paint_axis(ui, d, transform, true, &mut shapes);
            }
        }

        ui.painter().sub_region(*transform.frame()).extend(shapes);

        self.nodes
            .iter()
            .flat_map(|node| {
                node.ui(
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
                    &mut ui.child_ui(*transform.frame(), Layout::default()),
                    transform,
                )
            })
            .collect()
    }

    /// Interact with the UI and return the list of selected nodes
    fn interact(
        interactables: &[Interactable],
        selected_nodes: Vec<Id>,
        ui: &Ui,
        response: &Response,
    ) -> Vec<Id> {
        let mut selected_nodes = selected_nodes;
        if let Some(hover_pos) = response.hover_pos() {
            if response.clicked_by(PointerButton::Primary) {
                let node_interact_radius_sq: f32 = (16.0f32).powi(2);
                let node_edge_interact_radius_sq: f32 = (4.0f32).powi(2);
                let parameter_link_interact_radius_sq: f32 = (4.0f32).powi(2);

                let interactable = interactables
                    .iter()
                    .map(|interactable| {
                        let dist_sq = interactable.rect.distance_sq_to_pos(hover_pos);
                        (interactable, dist_sq)
                    })
                    .filter(
                        |(interactable, dist_sq)| match interactable.interaction_type {
                            InteractionType::Node => *dist_sq <= node_interact_radius_sq,
                            InteractionType::NodeEdge => *dist_sq <= node_edge_interact_radius_sq,
                            InteractionType::ParameterLinker(_) => {
                                *dist_sq <= parameter_link_interact_radius_sq
                            }
                        },
                    )
                    .min_by(|(interactable1, dist_sq1), (interactable2, dist_sq2)| {
                        interactable1
                            .interaction_type
                            .get_layer_order()
                            .cmp(&interactable2.interaction_type.get_layer_order())
                            .reverse()
                            .then(dist_sq1.ord().cmp(&dist_sq2.ord()))
                    })
                    .map(|(interactable, _)| interactable);

                if let Some(interactable) = interactable {
                    if matches!(interactable.interaction_type, InteractionType::Node) {
                        if ui.input().modifiers.is_none() {
                            // select only that node
                            selected_nodes = vec![interactable.node_id];
                        } else if ui.input().modifiers.shift_only() {
                            // add or remove from the selected nodes list
                            let selected_node_index =
                                selected_nodes.iter().enumerate().find_map(|(index, id)| {
                                    if *id == interactable.node_id {
                                        Some(index)
                                    } else {
                                        None
                                    }
                                });

                            if let Some(index) = selected_node_index {
                                if index == selected_nodes.len() - 1 {
                                    selected_nodes.remove(index);
                                } else {
                                    let selected_node = selected_nodes.remove(index);
                                    selected_nodes.push(selected_node);
                                }
                            } else {
                                selected_nodes.push(interactable.node_id);
                            }
                        }
                    }
                } else if ui.input().modifiers.is_none() {
                    // deselect all nodes
                    selected_nodes.clear();
                }
            }
        }
        selected_nodes
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
        let mut memory =
            NodeGraphMemory::load(ui.ctx(), node_graph_id).unwrap_or_else(|| NodeGraphMemory {
                auto_bounds: !self.min_auto_bounds.is_valid(),
                min_auto_bounds: self.min_auto_bounds,
                last_screen_transform: ScreenTransform::new(
                    rect,
                    self.min_auto_bounds,
                    center_x_axis,
                    center_y_axis,
                ),
                node_data: self
                    .nodes
                    .iter()
                    .map(|node| (node.id, NodeMemory::new(node.position, node.width)))
                    .collect(),
                selected_nodes: Vec::new(),
            });

        // If the min bounds changed, recalculate everything.
        if self.min_auto_bounds != memory.min_auto_bounds {
            memory = NodeGraphMemory {
                auto_bounds: !self.min_auto_bounds.is_valid(),
                min_auto_bounds: self.min_auto_bounds,
                ..memory
            };
            memory.clone().store(ui.ctx(), node_graph_id);
        }

        let NodeGraphMemory {
            mut auto_bounds,
            last_screen_transform,
            selected_nodes,
            node_data,
            ..
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

        // --- Bound computation ---
        let mut bounds = *last_screen_transform.bounds();

        // Allow double clicking to reset to automatic bounds.
        auto_bounds |= response.double_clicked_by(PointerButton::Primary);

        // Set bounds automatically based on content.
        if auto_bounds || !bounds.is_valid() {
            bounds = self.min_auto_bounds;
        }

        let transform = {
            let mut transform = ScreenTransform::new(rect, bounds, center_x_axis, center_y_axis);

            // Enforce equal aspect ratio.
            transform.set_aspect(1.0);

            // Dragging
            if response.dragged_by(PointerButton::Middle)
                || (ui.input().modifiers.alt_only() && response.dragged_by(PointerButton::Primary))
            {
                response = response.on_hover_cursor(CursorIcon::Grabbing);
                transform.translate_bounds(-response.drag_delta());
                auto_bounds = false;
            }

            // Zooming
            if let Some(hover_pos) = response.hover_pos() {
                let zoom_factor = Vec2::splat(ui.input().zoom_delta());
                if zoom_factor != Vec2::splat(1.0) {
                    transform.zoom(zoom_factor, hover_pos);
                    auto_bounds = false;
                }

                let scroll_delta = ui.input().scroll_delta;
                if scroll_delta != Vec2::ZERO {
                    transform.translate_bounds(-scroll_delta);
                    auto_bounds = false;
                }
            }

            transform
        };

        let interactables = self.ui(&selected_nodes, ui, &transform);

        let selected_nodes = Self::interact(&interactables, selected_nodes, ui, &response);

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

        let mut child_ui = ui.child_ui(rect, *ui.layout());

        let inner = add_contents(&mut child_ui);

        let memory = NodeGraphMemory {
            auto_bounds,
            min_auto_bounds: self.min_auto_bounds,
            last_screen_transform: transform,
            node_data: self
                .nodes
                .iter()
                .map(|node| (node.id, NodeMemory::new(node.position, node.width)))
                .collect(),
            selected_nodes,
        };
        memory.store(ui.ctx(), node_graph_id);

        InnerResponse { inner, response }
    }
}
