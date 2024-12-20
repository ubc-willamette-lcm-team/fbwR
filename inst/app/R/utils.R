
floating_buttons_html <- "
          .floating-element-r {
            position: fixed;
            top: 50px; /* Adjust as needed */
            right: 20px; /* Adjust as needed */
            z-index: 999; /* Ensures it's above other content */
          }
          .floating-element-l {
            position: fixed;
            top: 50px; /* Adjust as needed */
            left: 20px; /* Adjust as needed */
            z-index: 999; /* Ensures it's above other content */
          }
        "

# Custom heatmap renderer
renderer_heatmap_custom <- function(color_scale) {
  # This custom renderer renders the heatmap across ALL cells, not just
  # within a given column. Also uses a middle value for 1, neutral.
  renderer <- gsub("\n", "", "
      function (instance, td, row, col, prop, value, cellProperties) {

        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        heatmapScale  = chroma.scale(['%s1', '%s2']).mode('lch');

        if (instance.heatmap) {
          mn = instance.heatmap[col].min;
          mx = instance.heatmap[col].max;
          pt = (parseFloat(value, 10) - mn) / (mx - mn);

          td.style.backgroundColor = heatmapScale(pt).hex();
        }
      }
      ")
  renderer <- gsub("%s1", color_scale[1], renderer)
  renderer <- gsub("%s2", color_scale[2], renderer)
  renderer
}
