# Plot Tracks Colored By Attribute

MoveApps

Github repository: github.com/movestore/PlotTracksColoredByAttribute

## Description
Each segment of the plotted track(s) can be colored by the values of any of the available attributes associated to the locations or tracks. One or two attributes can be chosen. Individuals can be displayed on separate panels, or all in one.

## Documentation
This is an interactive App (shiny UI) to display and color the tracks on an interactive base map ("OpenStreetMap", "TopoMap"or "Aerial"), enabling the user to interactively select:
- the individuals to be display (select and unselect them)  
- to color by one or two attributes. When two attributes are selected, one of has to be categorical, and the other continuous.
- the color (gradient, palette, shade)  
- displayed panel (single or multiple)  
- style of plot (line width and transparency)  
- "Add columns color hex and legend in the returned data": if checked the columns `color_hex`, `attribute name(s)` will be added to the data. If e.g. the attributes 'sex' and 'speed' where selected, the column added will be named `sex-speed` and will contain the values of each attribute separated by `-`. This might be useful to e.g. recreate the plot outside of MoveApps

The created plot can than be saved locally as HTML or PNG via `Save Map as HTML` and `Save Map as PNG` buttons.
 
### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. 

#### Required data properties
The App should work for any kind of (location) data.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
This App does not produce Artefacts. The following files can be downloaded optionally:  

Plots_HTML_xx.html : representing map and plot as HTML in single panel  

Plots_HTML_xx.zip : representing map and plot as zip folder containing different single map and plot as a HTML for each track  

Plots_PNG_xx.PNG : representing map and plot as HTML in single panel  

Plots_PNG_xx.zip : representing map and plot as zip folder containing different single map and plot as a PNG for each track


### Settings
**"Tracks"**: select one or multiple individuals. Buttons available to select or unselect all tracks.

**"Attribute"**: select the "Option 1: Color by 1 attribute" or "Option 2: Color by 2 attributes" 
 All available attributes of the study which contain data are displayed.
 
 **`Option 1`**:  
 Select the attribute from the drop down list. Attributes can be continous and categorical.  
    -if attribute is continuous: `Colors`: select the `Low` and `High` color from gradient.  
    -if attribute is categorical : `Colors` :select the pallete from the dropdown.
    *note 1: numeric attributes with fewer than 12 unique values are treated as categorical.  
    Note 1: For categorical attributes with many levels, the app automatically generates additional color tones to distinguish them more clearly.
    
 **`Option 2`**:  
   `Categorical Attribute` : select categorical attribute from the drop down list  
   `Palette`: select from the drop down list  
   `Continuous Attribute` : select continuous attribute from the drop down list  
   `Shade`: select one of these options from the drop down list: "light to dark"" or "dark to light""
   
**"Panel"**:  
  The tracks can be either displayed on :  
  `Single panel`: shows all track's plot on one panel  
  `Multipanel` : shows each track on separate panels.
    
    
**"Style"** : user can customize the style of plot lines: `Line width`  & `Transparency`

Check box `Add columns color hex and attribute name(s) in the returned data`:  
The App adds three columns to the returned move2 object:      
track_id : the track identifier;      
color_hex : the hex code used to draw each point/segment;      
'attribute name(s)' : when `option 1` is selected, this column will be called 'color_legend_xx' being xx the name of the selected attribute, and containing the values of this attribute. When `option 2` is selected, this column will be called 'attr1-attr2' ie the name of both attributes separated by `-`, and will contain the values of both attributes separated by `-`.

**"Download"**:
`Save Map as HTML`: locally downloads the current plot in HTML format.
`Save Map as PNG`:  locally downloads the current plot in PNG format.
*Note: In Multipanel mode, a ZIP archive is created with one HTML/PNG per track.

**"Map Selection"**: User can select three types op maps:  `OpenStreetMap` or `TopoMap` or `Aerial`  

**“Zoom in–Zoom out”**: use the mouse wheel and the +/- controls; double-click to zoom in, and double-click the map background again to reset the view.

**"Apply changes"**: button to apply any selection


### Changes in output data

If the user ticked `Add columns color hex and legend in the returned data`, the App returns the input data and adds three columns corresponding the to `track_id`, the `color_hex` and a columns referring to the chosen attributes that will vary in name (for details see the Settings section above)


### Null or error handling

**No animal selected** : message “Please select one or more animals.”  
**Data**: For use in further Apps the input data set is returned unmodified. Empty input will give an error.  
**Tracks** : with fewer than 2 points or no consecutive points return empty sf objects  
**Missing attributes value**: are shown in light gray  
**empty segments**: “No segments for selected animals.”  
**Downloads**: skip when data or selections are empty.


