use epaint::{emath::NumExt, Vec2};

use crate::{
    plot::transform::{PlotBounds, ScreenTransform},
    Context, CursorIcon, Id, InnerResponse, PointerButton, Sense, Ui,
};

/// Information about the node graph that has to persist between frames.
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Clone)]
struct NodeGraphMemory {
    auto_bounds: bool,
    min_auto_bounds: PlotBounds,
    last_screen_transform: ScreenTransform,
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

    fn ui(&self, ui: &mut Ui, transform: &ScreenTransform) {
        let mut shapes = Vec::new();

        if self.show_axis {
            for d in 0..2 {
                crate::paint_axis(ui, d, transform, false, &mut shapes);
            }
        }

        ui.painter().sub_region(*transform.frame()).extend(shapes);
    }

    pub fn show<R>(self, ui: &mut Ui, add_contents: impl FnOnce(&mut Ui) -> R) -> InnerResponse<R> {
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
            ..
        } = memory;

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
            if response.dragged_by(PointerButton::Primary) {
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

        self.ui(ui, &transform);

        let mut child_ui = ui.child_ui(rect, ui.layout().clone());

        let inner = add_contents(&mut child_ui);

        let memory = NodeGraphMemory {
            auto_bounds,
            min_auto_bounds: self.min_auto_bounds,
            last_screen_transform: transform,
        };
        memory.store(ui.ctx(), node_graph_id);

        InnerResponse { inner, response }
    }
}
