//! Widgets are pieces of GUI such as [`Label`], [`Button`], [`Slider`] etc.
//!
//! Example widget uses:
//! * `ui.add(Label::new("Text").text_color(color::red));`
//! * `if ui.add(Button::new("Click me")).clicked() { … }`

use crate::*;

mod button;
pub mod color_picker;
pub(crate) mod drag_value;
mod hyperlink;
mod image;
mod label;
mod node_graph;
pub mod plot;
mod progress_bar;
mod selected_label;
mod separator;
mod slider;
mod spinner;
pub mod text_edit;

pub use button::*;
pub use drag_value::DragValue;
pub use hyperlink::*;
pub use image::Image;
pub use label::*;
pub use node_graph::NodeGraph;
pub use progress_bar::ProgressBar;
pub use selected_label::SelectableLabel;
pub use separator::Separator;
pub use slider::*;
pub use spinner::*;
pub use text_edit::{TextBuffer, TextEdit};

// ----------------------------------------------------------------------------

/// Anything implementing Widget can be added to a [`Ui`] with [`Ui::add`].
///
/// [`Button`], [`Label`], [`Slider`], etc all implement the `Widget` trait.
///
/// Note that the widgets (`Button`, `TextEdit` etc) are
/// [builders](https://doc.rust-lang.org/1.0.0/style/ownership/builders.html),
/// and not objects that hold state.
///
/// Tip: you can `impl Widget for &mut YourThing { }`.
///
/// `|ui: &mut Ui| -> Response { … }` also implements `Widget`.
#[must_use = "You should put this widget in an ui with `ui.add(widget);`"]
pub trait Widget {
    /// Allocate space, interact, paint, and return a [`Response`].
    ///
    /// Note that this consumes `self`.
    /// This is because most widgets ([`Button`], [`TextEdit`] etc) are
    /// [builders](https://doc.rust-lang.org/1.0.0/style/ownership/builders.html)
    ///
    /// Tip: you can `impl Widget for &mut YourObject { }`.
    fn ui(self, ui: &mut Ui) -> Response;
}

/// This enables functions that return `impl Widget`, so that you can
/// create a widget by just returning a lambda from a function.
///
/// For instance: `ui.add(slider_vec2(&mut vec2));` with:
///
/// ```
/// pub fn slider_vec2(value: &mut egui::Vec2) -> impl egui::Widget + '_ {
///    move |ui: &mut egui::Ui| {
///        ui.horizontal(|ui| {
///            ui.add(egui::Slider::new(&mut value.x, 0.0..=1.0).text("x"));
///            ui.add(egui::Slider::new(&mut value.y, 0.0..=1.0).text("y"));
///        })
///        .response
///    }
/// }
/// ```
impl<F> Widget for F
where
    F: FnOnce(&mut Ui) -> Response,
{
    fn ui(self, ui: &mut Ui) -> Response {
        self(ui)
    }
}

/// Helper so that you can do `TextEdit::State::read…`
pub trait WidgetWithState {
    type State;
}

// ----------------------------------------------------------------------------

/// Show a button to reset a value to its default.
/// The button is only enabled if the value does not already have its original value.
pub fn reset_button<T: Default + PartialEq>(ui: &mut Ui, value: &mut T) {
    reset_button_with(ui, value, T::default());
}
/// Show a button to reset a value to its default.
/// The button is only enabled if the value does not already have its original value.
pub fn reset_button_with<T: PartialEq>(ui: &mut Ui, value: &mut T, reset_value: T) {
    if ui
        .add_enabled(*value != reset_value, Button::new("Reset"))
        .clicked()
    {
        *value = reset_value;
    }
}

// ----------------------------------------------------------------------------

pub fn stroke_ui(ui: &mut crate::Ui, stroke: &mut epaint::Stroke, text: &str) {
    let epaint::Stroke { width, color } = stroke;
    ui.horizontal(|ui| {
        ui.add(DragValue::new(width).speed(0.1).clamp_range(0.0..=5.0))
            .on_hover_text("Width");
        ui.color_edit_button_srgba(color);
        ui.label(text);

        // stroke preview:
        let (_id, stroke_rect) = ui.allocate_space(ui.spacing().interact_size);
        let left = stroke_rect.left_center();
        let right = stroke_rect.right_center();
        ui.painter().line_segment([left, right], (*width, *color));
    });
}

pub(crate) fn shadow_ui(ui: &mut Ui, shadow: &mut epaint::Shadow, text: &str) {
    let epaint::Shadow { extrusion, color } = shadow;
    ui.horizontal(|ui| {
        ui.label(text);
        ui.add(
            DragValue::new(extrusion)
                .speed(1.0)
                .clamp_range(0.0..=100.0),
        )
        .on_hover_text("Extrusion");
        ui.color_edit_button_srgba(color);
    });
}

/// Show a small button to switch to/from dark/light mode (globally).
pub fn global_dark_light_mode_switch(ui: &mut Ui) {
    let style: crate::Style = (*ui.ctx().style()).clone();
    let new_visuals = style.visuals.light_dark_small_toggle_button(ui);
    if let Some(visuals) = new_visuals {
        ui.ctx().set_visuals(visuals);
    }
}

/// Show larger buttons for switching between light and dark mode (globally).
pub fn global_dark_light_mode_buttons(ui: &mut Ui) {
    let mut visuals = ui.ctx().style().visuals.clone();
    visuals.light_dark_radio_buttons(ui);
    ui.ctx().set_visuals(visuals);
}

pub(crate) fn paint_axis(
    ui: &Ui,
    axis: usize,
    transform: &plot::transform::ScreenTransform,
    show_text: bool,
    shapes: &mut Vec<Shape>,
) {
    let bounds = transform.bounds();
    let text_style = TextStyle::Body;

    let base: i64 = 10;
    let basef = base as f64;

    let min_line_spacing_in_points = 6.0; // TODO: large enough for a wide label
    let step_size = transform.dvalue_dpos()[axis] * min_line_spacing_in_points;
    let step_size = basef.powi(step_size.abs().log(basef).ceil() as i32);

    let step_size_in_points = (transform.dpos_dvalue()[axis] * step_size).abs() as f32;

    // Where on the cross-dimension to show the label values
    let value_cross = 0.0_f64.clamp(bounds.min[1 - axis], bounds.max[1 - axis]);

    for i in 0.. {
        let value_main = step_size * (bounds.min[axis] / step_size + i as f64).floor();
        if value_main > bounds.max[axis] {
            break;
        }

        let value = if axis == 0 {
            plot::Value::new(value_main, value_cross)
        } else {
            plot::Value::new(value_cross, value_main)
        };
        let pos_in_gui = transform.position_from_value(&value);

        let n = (value_main / step_size).round() as i64;
        let spacing_in_points = if n % (base * base) == 0 {
            step_size_in_points * (basef * basef) as f32 // think line (multiple of 100)
        } else if n % base == 0 {
            step_size_in_points * basef as f32 // medium line (multiple of 10)
        } else {
            step_size_in_points // thin line
        };

        let line_alpha = remap_clamp(
            spacing_in_points,
            (min_line_spacing_in_points as f32)..=300.0,
            0.0..=0.15,
        );

        if line_alpha > 0.0 {
            let line_color = color_from_alpha(ui, line_alpha);

            let mut p0 = pos_in_gui;
            let mut p1 = pos_in_gui;
            p0[1 - axis] = transform.frame().min[1 - axis];
            p1[1 - axis] = transform.frame().max[1 - axis];
            shapes.push(Shape::line_segment([p0, p1], Stroke::new(1.0, line_color)));
        }

        if show_text {
            let text_alpha = remap_clamp(spacing_in_points, 40.0..=150.0, 0.0..=0.4);

            if text_alpha > 0.0 {
                let color = color_from_alpha(ui, text_alpha);
                let text = emath::round_to_decimals(value_main, 5).to_string(); // hack

                let galley = ui.painter().layout_no_wrap(text, text_style, color);

                let mut text_pos = pos_in_gui + vec2(1.0, -galley.size().y);

                // Make sure we see the labels, even if the axis is off-screen:
                text_pos[1 - axis] = text_pos[1 - axis]
                    .at_most(transform.frame().max[1 - axis] - galley.size()[1 - axis] - 2.0)
                    .at_least(transform.frame().min[1 - axis] + 1.0);

                shapes.push(Shape::galley(text_pos, galley));
            }
        }
    }

    fn color_from_alpha(ui: &Ui, alpha: f32) -> Color32 {
        if ui.visuals().dark_mode {
            Rgba::from_white_alpha(alpha).into()
        } else {
            Rgba::from_black_alpha((4.0 * alpha).at_most(1.0)).into()
        }
    }
}
