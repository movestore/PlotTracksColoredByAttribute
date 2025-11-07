# Plot Tracks Colored By Attribute

MoveApps

Github repository: github.com/movestore/PlotTracksColoredByAttribute

## Description
Each segment of the plotted track(s) can be colored by the values of any of the available attributes associated to the locations. attributes could be continuous or categorical. for categorical  Single or multiple individuals can be displayed simultaneously.

## Documentation
This App is embedded in an shiny UI, enabling the user to interactively select the attribute by which the segments of the track should be colored, the individuals to be display, the color gradient and if the tracks should be displayed all in one panel, or in multiple panels (one per individual). It is possible to zoom in to a plot by selecting an area with the mouse and double click. To go back to the full plot, double click again.

The created plot can than be saved locally via the `Save plot` button. By default the size of the saved plot depends on the screen of the users device. Optionally the plot can be saved with a width and height set by the user. 

The background map only displays country border lines. If your data is only within one country, you will just see a grey background. More detailed background maps to be added soon.


"Our Shiny app visualizes animal movement tracks on an interactive Leaflet map and lets you color those tracks by their metadata. You can select which animals to include, and then choose how to color the tracks.

There are two visualization modes. In Option 1, we color the track using a single attribute. If that attribute is continuous, like speed or altitude, we use a color gradient. If it's categorical, like species or behavior state, each category gets its own color and we generate a matching legend automatically.

In Option 2, we combine two attributes: a categorical attribute defines the base color, and then a continuous attribute is used to shade that color lighter or darker. So for example, one species could always be blue, but within that species darker blue means higher speed. That way we can encode two dimensions of information on a single line.

The app also supports single-panel and multi-panel display. In single-panel mode we plot all selected tracks together. In multi-panel mode, we generate one Leaflet map per animal.

For reproducibility and export, the app can save the map as HTML or PNG. For multi-panel, it exports one file per animal and zips them. We also have an option to attach the final color assignments back into the data. When you enable that, the app adds columns with the actual hex color and the legend values, and you can download that as a CSV. That means downstream tools can reuse exactly the same visual encoding."


### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
This App does not produce Artefacts. The following files can be downloaded optionally:
Plots_HTML_xx.html : representing map and plot as HTML in single panel
Plots_HTML_xx.zip : representing map and plot as zip folder containing different single map and plot as a HTML for each track
Plots_PNG_xx.PNG : representing map and plot as HTML in single panel
Plots_PNG_xx.zip : representing map and plot as zip folder containing different single map and plot as a PNG for each track

### Settings
`Tracks`: select one or multiple individuals. select or unselect all tracks could be done by related button.

`Attribute`: first select the "Option 1: Color by 1 attribute" or "Option 2: Color by 2 attributes" 
 All available attributes associated to the locations of the study are displayed.Then select: 
 Option 1: select the attribute from the drop down list.
    -if attribute is continuous: `Colors`:select the `Low` and `High` color from gradient.
    -if attribute is categorical : `Colors` :select the pallete from the dropdown
    *note: numeric attributes with fewer than 12 unique values treated as categorical.
    
 Option 2: 
   `Categorical Attribute` : select from the drop down list
   `Palette`: select from the drop down list
   `Continuous Attribute` : select from the drop down list
   `Shade`: select from the drop down list (light to dark- dark to light)
   
`Panel`: the tracks can be either displayed on a `Single panel` or on a `Multipanel`, were each individual is displayed on one panel. Currently the plots in the multipanel display.



`Save Plot`: locally downloads the current plot.

`Width(mm)` & `Height(mm)`: optional. Save plot with personalized width and hight in mm.


### Changes in output data
if user select the checkbox "Add columns color hex and legend in the returned data", it returns the data set with two additional columns: color hexadecimal and the column of attribute that colored the plot based on it. 


### Null or error handling
**No animal selected** : message “Please select one or more animals.”
**Data**: For use in further Apps the input data set is returned unmodified. Empty input will give an error.
**Tracks** : with fewer than 2 points or no consecutive points return empty sf objects
**Missing attributes value**: are shown in light gray
**No animal selected** : message “Please select one or more animals.”
**empty segments**: “No segments for selected animals.”
**Downloads**: skip when data or selections are empty.
