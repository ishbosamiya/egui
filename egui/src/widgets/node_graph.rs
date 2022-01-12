use std::hash::Hash;

use epaint::{
    emath::{Align2, NumExt},
    util::FloatOrd,
    Pos2, Rect, Shape, TextStyle, Vec2,
};

use crate::{
    plot::transform::{PlotBounds, ScreenTransform},
    Context, CursorIcon, Id, IdMap, InnerResponse, Layout, PointerButton, Response, Sense, Ui,
    WidgetText,
};

pub struct Parameter {
    name: WidgetText,
}

impl Parameter {
    pub fn new(name: impl Into<WidgetText>) -> Self {
        Self { name: name.into() }
    }
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

    /// Draw the ui and return the final footprint of the ui for
    /// further interaction tests
    fn ui(&self, selected: bool, ui: &mut Ui, transform: &ScreenTransform) -> Rect {
        ui.set_clip_rect(*transform.frame());

        let color = ui.style().visuals.text_color();
        let heading_galley =
            ui.fonts()
                .layout_no_wrap(self.name.text().to_string(), TextStyle::Heading, color);

        let initial_pos = transform.transformed_pos(&self.position);
        let pos = initial_pos + Vec2::new(0.0, heading_galley.size().y);

        let mut shapes = Vec::new();

        let mut inputs_param_size_x = None;
        let mut inputs_param_size_y = 0.0;
        self.inputs.iter().for_each(|(_id, input)| {
            let text_style = TextStyle::Body;
            let row_height = ui.fonts().row_height(text_style);
            let galley =
                ui.fonts()
                    .layout_no_wrap(input.name.text().to_string(), text_style, color);
            let rect = Align2::LEFT_TOP.anchor_rect(Rect::from_min_size(
                pos + Vec2::new(0.0, inputs_param_size_y),
                galley.size(),
            ));

            inputs_param_size_x = if let Some(inputs_param_size_x) = inputs_param_size_x {
                Some(galley.size().x.max(inputs_param_size_x))
            } else {
                Some(galley.size().x)
            };
            inputs_param_size_y += row_height;

            shapes.push(Shape::galley(rect.min, galley));
        });
        let inputs_param_size = Vec2::new(inputs_param_size_x.unwrap_or(0.0), inputs_param_size_y);

        let gap = 25.0;
        let pos = pos + Vec2::new(inputs_param_size.x + gap, 0.0);

        let mut outputs_param_size_x = None;
        let mut outputs_param_size_y = 0.0;
        self.outputs.iter().for_each(|(_id, output)| {
            let text_style = epaint::TextStyle::Body;
            let row_height = ui.fonts().row_height(text_style);
            let galley =
                ui.fonts()
                    .layout_no_wrap(output.name.text().to_string(), text_style, color);
            let rect = Align2::LEFT_TOP.anchor_rect(Rect::from_min_size(
                pos + Vec2::new(0.0, outputs_param_size_y),
                galley.size(),
            ));

            outputs_param_size_x = if let Some(outputs_param_size_x) = outputs_param_size_x {
                Some(galley.size().x.max(outputs_param_size_x))
            } else {
                Some(galley.size().x)
            };
            outputs_param_size_y += row_height;

            shapes.push(Shape::galley(rect.min, galley));
        });
        let outputs_param_size =
            Vec2::new(outputs_param_size_x.unwrap_or(0.0), outputs_param_size_y);

        let heading_rect = Align2::CENTER_TOP.anchor_rect(Rect::from_min_size(
            initial_pos
                + Vec2::new(
                    (inputs_param_size.x + gap + outputs_param_size.x) * 0.5,
                    0.0,
                ),
            heading_galley.size(),
        ));
        shapes.push(Shape::galley(heading_rect.min, heading_galley));

        let background_rect = Rect::from_min_size(
            initial_pos,
            Vec2::new(
                inputs_param_size.x + gap + outputs_param_size.x,
                inputs_param_size.y.max(outputs_param_size.y) + heading_rect.height(),
            ),
        );

        ui.painter()
            .rect_filled(background_rect, 2.0, ui.style().visuals.faint_bg_color);
        if selected {
            ui.painter().rect_stroke(
                background_rect.expand(ui.spacing().button_padding.x),
                2.0,
                ui.visuals().selection.stroke,
            );
        }

        let ui = ui.child_ui(background_rect, Layout::default());

        ui.painter().sub_region(*transform.frame()).extend(shapes);

        background_rect
    }
}

/// Information about the node graph that has to persist between frames.
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone)]
struct NodeGraphMemory {
    auto_bounds: bool,
    min_auto_bounds: PlotBounds,
    last_screen_transform: ScreenTransform,
    node_positions: IdMap<Pos2>,
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

    /// Setup the UI.
    ///
    /// Returns the Node(s) that must be selected for the next frame,
    /// if no valid (wrt the node graph) interaction takes place, the
    /// current selected node list is returned.
    fn ui(
        &self,
        selected_nodes: Vec<Id>,
        ui: &mut Ui,
        response: &Response,
        transform: &ScreenTransform,
    ) -> Vec<Id> {
        let mut shapes = Vec::new();

        if self.show_axis {
            for d in 0..2 {
                crate::paint_axis(ui, d, transform, false, &mut shapes);
            }
        }

        ui.painter().sub_region(*transform.frame()).extend(shapes);

        let node_rects: Vec<Rect> = self
            .nodes
            .iter()
            .map(|node| {
                node.ui(
                    selected_nodes
                        .iter()
                        .any(|selected_node| *selected_node == node.id),
                    &mut ui.child_ui(*transform.frame(), Layout::default()),
                    transform,
                )
            })
            .collect();

        if let Some(hover_pos) = response.hover_pos() {
            if response.clicked_by(PointerButton::Primary) {
                let interact_radius_sq: f32 = (16.0f32).powi(2);

                let selected_node = node_rects
                    .iter()
                    .enumerate()
                    .map(|(i, rect)| {
                        let dist_sq = rect.distance_sq_to_pos(hover_pos);
                        (i, dist_sq)
                    })
                    .min_by_key(|(_, dist_sq)| dist_sq.ord())
                    .and_then(|(index, dist_sq)| {
                        if dist_sq <= interact_radius_sq {
                            Some(index)
                        } else {
                            None
                        }
                    })
                    .map(|index| self.nodes[index].id);

                if ui.input().modifiers.is_none() {
                    // select only one node, or deselect all nodes
                    if let Some(selected_node) = selected_node {
                        vec![selected_node]
                    } else {
                        Vec::new()
                    }
                } else if ui.input().modifiers.shift_only() {
                    if let Some(selected_node) = selected_node {
                        // add or remove from the selected nodes list
                        let selected_node_index =
                            selected_nodes.iter().enumerate().find_map(|(index, id)| {
                                if *id == selected_node {
                                    Some(index)
                                } else {
                                    None
                                }
                            });
                        let mut selected_nodes = selected_nodes;
                        if let Some(index) = selected_node_index {
                            selected_nodes.swap_remove(index);
                        } else {
                            selected_nodes.push(selected_node);
                        }
                        selected_nodes
                    } else {
                        // do not change selected nodes list
                        selected_nodes
                    }
                } else {
                    selected_nodes
                }
            } else {
                selected_nodes
            }
        } else {
            selected_nodes
        }
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
        let (rect, mut response) = ui.allocate_exact_size(size, Sense::drag());

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
                node_positions: self
                    .nodes
                    .iter()
                    .map(|node| (node.id, node.position))
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
            node_positions,
            ..
        } = memory;

        // force nodes to their respective positions
        self.nodes.iter_mut().for_each(|node| {
            if let Some(position) = node_positions.get(&node.id) {
                node.position = *position;
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

        let selected_nodes = self.ui(selected_nodes, ui, &response, &transform);

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
            node_positions: self
                .nodes
                .iter()
                .map(|node| (node.id, node.position))
                .collect(),
            selected_nodes,
        };
        memory.store(ui.ctx(), node_graph_id);

        InnerResponse { inner, response }
    }
}
