# Plot Tracks Colored By Attribute

MoveApps

Github repository: github.com/movestore/PlotTracksColoredByAttribute

## Description
Each segment of the plotted track(s) can be colored by the values of any of the available attributes associated to the locations.  Single or multiple individuals can be displayed simultaneously.

## Documentation
This App is embedded in an shiny UI to display and color the tracks on an interactive Leaflet map, enabling the user to interactively select:
-the individuals to be display(select and unselect them)  
-colored by one or two attributes  
-the attribute by which the segments of the track should be colored  
-the color(gradient, palette, shade)  
-displayed panel (single or multiple)  
-Style of plot (Line width and Transparency)  

user can select the "Add columns color hex and legend in the returned data" check box to add three columns—`track_id`, `color_hex`, `legend` to returned data to allow the results to be saved as an RDS or passed on to a subsequent app to create a shapefile, kml, etc.  
The background map only displays in three types.  
The created plot can than be saved locally as HTML or PNG via `Save Map as HTML` and `Save Map as PNG` buttons.
 

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
**"Tracks"**: select one or multiple individuals. select or unselect all tracks could be done by related button.

**"Attribute"**: select the "Option 1: Color by 1 attribute" or "Option 2: Color by 2 attributes" 
 All available attributes of the study that don't have NA columns, are displayed. All-NA columns are removed.
 
 **`Option 1`**:  
 select the attribute from the drop down list. this is mixed of continous and categorical attributes  
    -if attribute is continuous: `Colors`:select the `Low` and `High` color from gradient.  
    -if attribute is categorical : `Colors` :select the pallete from the dropdown  
    *note 1: numeric attributes with fewer than 12 unique values treated as categorical.  
    Note 1: For categorical attributes with many levels, the app automatically generates additional color tones to distinguish them more clearly.
    
 **`Option 2`**:  
   `Categorical Attribute` : select categorical attribute from the drop down list  
   `Palette`: select from the drop down list  
   `Continuous Attribute` : select continuous attribute from the drop down list  
   `Shade`: select one of these options from the drop down list: "light to dark"" or "dark to light""
   
**"Panel"**:  
  the tracks can be either displayed on :  
  `Single panel`: shows all track's plot on one panel  
  `Multipanel` :shows each track  on one panel.
    
    
**"Style"** : user can customize the style of plot lines: `Line width`  & `Transparency`

Check box `Add columns color hex and legend in the returned data`:  
the app adds three columns to the returned move2 object:
track_id : the track identifier; 
color_hex : the hex code used to draw each point/segment;
legend : the attribute values that the color represents (for one or two selected attributes)

**"Download"**:
`Save Map as HTML`: locally downloads the current plot in HTML format.
`Save Map as PNG`:  locally downloads the current plot in PNG format.
*Note: In Multipanel mode, a ZIP archive is created with one HTML/PNG per track.

**"Map Selection"**: User can select three types op maps:  `OpenStreetMap` or `TopoMap` or `Aerial`  

**“Zoom in–Zoom out”**: use the mouse wheel and the +/- controls; double-click to zoom in, and double-click the map background again to reset the view.



### Changes in output data

If user tick `Add columns color hex and legend in the returned data`, the app returns data plus a `colorhex`  column and a legend column `color_legend` ; otherwise it returns the data unchanged.


### Null or error handling

**No animal selected** : message “Please select one or more animals.”  
**Data**: For use in further Apps the input data set is returned unmodified. Empty input will give an error.  
**Tracks** : with fewer than 2 points or no consecutive points return empty sf objects  
**Missing attributes value**: are shown in light gray  
**No animal selected** : message “Please select one or more animals.”  
**empty segments**: “No segments for selected animals.”  
**Downloads**: skip when data or selections are empty.


