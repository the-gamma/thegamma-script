// AUTO-GENERATED - DO NOT EDIT
[<ReflectedDefinition;AutoOpen>]
module TheGamma.GoogleCharts.Extensions

open System
open TheGamma.GoogleCharts
open TheGamma.GoogleCharts.Helpers
open TheGamma.GoogleCharts.Options

type Geo = 
  { data : ChartData; typeName : string; 
    options : GeoChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?backgroundColor:obj,?datalessRegionColor:string,?displayMode:string,?enableRegionInteractivity:bool,?height:float,?keepAspectRatio:bool,?region:string,?markerOpacity:float,?resolution:string,?width:float) = 
    let o = x.options
    let newOptions = { x.options with backgroundColor = right o "backgroundColor" backgroundColor; datalessRegionColor = right o "datalessRegionColor" datalessRegionColor; displayMode = right o "displayMode" displayMode; enableRegionInteractivity = right o "enableRegionInteractivity" enableRegionInteractivity; height = right o "height" height; keepAspectRatio = right o "keepAspectRatio" keepAspectRatio; region = right o "region" region; markerOpacity = right o "markerOpacity" markerOpacity; resolution = right o "resolution" resolution; width = right o "width" width }
    { x with options = newOptions }
  member x.colorAxis(?minValue:float,?maxValue:float,?values:seq<float>,?colors:seq<string>) =
    let o = x.options.colorAxis
    let newNested = { ChartColorAxis.minValue = right o "minValue" minValue; maxValue = right o "maxValue" maxValue; values = right o "values" (Option.map Array.ofSeq values); colors = right o "colors" (Option.map Array.ofSeq colors); legend = copy o "legend" }
    { x with options = { x.options with colorAxis = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.magnifyingGlass(?enable:bool,?zoomFactor:float) =
    let o = x.options.magnifyingGlass
    let newNested = { GeoChartMagnifyingGlass.enable = right o "enable" enable; zoomFactor = right o "zoomFactor" zoomFactor;  }
    { x with options = { x.options with magnifyingGlass = newNested } }
  member x.sizeAxis(?maxSize:float,?maxValue:float,?minSize:float,?minValue:float) =
    let o = x.options.sizeAxis
    let newNested = { ChartSizeAxis.maxSize = right o "maxSize" maxSize; maxValue = right o "maxValue" maxValue; minSize = right o "minSize" minSize; minValue = right o "minValue" minValue;  }
    { x with options = { x.options with sizeAxis = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
type Scatter = 
  { data : ChartData; typeName : string; 
    options : ScatterChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?aggregationTarget:string,?axisTitlesPosition:string,?backgroundColor:obj,?colors:seq<string>,?curveType:string,?dataOpacity:float,?enableInteractivity:bool,?fontSize:float,?fontName:string,?forceIFrame:bool,?height:float,?lineWidth:float,?pointSize:float,?selectionMode:string,?series:obj,?theme:string,?title:string,?titlePosition:string,?width:float) = 
    let o = x.options
    let newOptions = { x.options with aggregationTarget = right o "aggregationTarget" aggregationTarget; axisTitlesPosition = right o "axisTitlesPosition" axisTitlesPosition; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); curveType = right o "curveType" curveType; dataOpacity = right o "dataOpacity" dataOpacity; enableInteractivity = right o "enableInteractivity" enableInteractivity; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; forceIFrame = right o "forceIFrame" forceIFrame; height = right o "height" height; lineWidth = right o "lineWidth" lineWidth; pointSize = right o "pointSize" pointSize; selectionMode = right o "selectionMode" selectionMode; series = right o "series" series; theme = right o "theme" theme; title = right o "title" title; titlePosition = right o "titlePosition" titlePosition; width = right o "width" width }
    { x with options = newOptions }
  member x.trendlines(?trendlines:seq<Trendline>) =
    let o = x.options
    { x with options = { x.options with trendlines = right o "trendlines" (Option.map Array.ofSeq trendlines) } }
  member x.animation(?duration:float,?easing:string) =
    let o = x.options.animation
    let newNested = { TransitionAnimation.duration = right o "duration" duration; easing = right o "easing" easing;  }
    { x with options = { x.options with animation = newNested } }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.crosshair(?color:string,?opacity:float,?orientation:string,?trigger:string) =
    let o = x.options.crosshair
    let newNested = { ChartCrosshair.color = right o "color" color; opacity = right o "opacity" opacity; orientation = right o "orientation" orientation; trigger = right o "trigger" trigger; focused = copy o "focused"; selected = copy o "selected" }
    { x with options = { x.options with crosshair = newNested } }
  member x.explorer(?actions:seq<string>,?axis:string,?keepInBounds:bool,?maxZoomIn:float,?maxZoomOut:float,?zoomDelta:float) =
    let o = x.options.explorer
    let newNested = { ChartExplorer.actions = right o "actions" (Option.map Array.ofSeq actions); axis = right o "axis" axis; keepInBounds = right o "keepInBounds" keepInBounds; maxZoomIn = right o "maxZoomIn" maxZoomIn; maxZoomOut = right o "maxZoomOut" maxZoomOut; zoomDelta = right o "zoomDelta" zoomDelta;  }
    { x with options = { x.options with explorer = newNested } }
  member x.hAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.hAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with hAxis = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
  member x.vAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.vAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with vAxis = newNested } }
type Column = 
  { data : ChartData; typeName : string; 
    options : ColumnChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?aggregationTarget:string,?axisTitlesPosition:string,?backgroundColor:obj,?colors:seq<string>,?enableInteractivity:bool,?focusTarget:string,?fontSize:float,?fontName:string,?height:float,?isStacked:bool,?reverseCategories:bool,?selectionMode:string,?series:obj,?theme:string,?title:string,?titlePosition:string,?vAxes:obj,?width:float) = 
    let o = x.options
    let newOptions = { x.options with aggregationTarget = right o "aggregationTarget" aggregationTarget; axisTitlesPosition = right o "axisTitlesPosition" axisTitlesPosition; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); enableInteractivity = right o "enableInteractivity" enableInteractivity; focusTarget = right o "focusTarget" focusTarget; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; height = right o "height" height; isStacked = right o "isStacked" isStacked; reverseCategories = right o "reverseCategories" reverseCategories; selectionMode = right o "selectionMode" selectionMode; series = right o "series" series; theme = right o "theme" theme; title = right o "title" title; titlePosition = right o "titlePosition" titlePosition; vAxes = right o "vAxes" vAxes; width = right o "width" width }
    { x with options = newOptions }
  member x.animation(?duration:float,?easing:string) =
    let o = x.options.animation
    let newNested = { TransitionAnimation.duration = right o "duration" duration; easing = right o "easing" easing;  }
    { x with options = { x.options with animation = newNested } }
  member x.bar(?groupWidth:obj) =
    let o = x.options.bar
    let newNested = { GroupWidth.groupWidth = right o "groupWidth" groupWidth;  }
    { x with options = { x.options with bar = newNested } }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.hAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.hAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with hAxis = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
  member x.vAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.vAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with vAxis = newNested } }
type Line = 
  { data : ChartData; typeName : string; 
    options : LineChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?aggregationTarget:string,?axisTitlesPosition:string,?backgroundColor:obj,?colors:seq<string>,?curveType:string,?dataOpacity:float,?enableInteractivity:bool,?focusTarget:string,?fontSize:float,?fontName:string,?height:float,?interpolateNulls:bool,?lineWidth:float,?orientation:string,?pointSize:float,?reverseCategories:bool,?selectionMode:string,?series:obj,?theme:string,?title:string,?titlePosition:string,?vAxes:obj,?width:float) = 
    let o = x.options
    let newOptions = { x.options with aggregationTarget = right o "aggregationTarget" aggregationTarget; axisTitlesPosition = right o "axisTitlesPosition" axisTitlesPosition; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); curveType = right o "curveType" curveType; dataOpacity = right o "dataOpacity" dataOpacity; enableInteractivity = right o "enableInteractivity" enableInteractivity; focusTarget = right o "focusTarget" focusTarget; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; height = right o "height" height; interpolateNulls = right o "interpolateNulls" interpolateNulls; lineWidth = right o "lineWidth" lineWidth; orientation = right o "orientation" orientation; pointSize = right o "pointSize" pointSize; reverseCategories = right o "reverseCategories" reverseCategories; selectionMode = right o "selectionMode" selectionMode; series = right o "series" series; theme = right o "theme" theme; title = right o "title" title; titlePosition = right o "titlePosition" titlePosition; vAxes = right o "vAxes" vAxes; width = right o "width" width }
    { x with options = newOptions }
  member x.animation(?duration:float,?easing:string) =
    let o = x.options.animation
    let newNested = { TransitionAnimation.duration = right o "duration" duration; easing = right o "easing" easing;  }
    { x with options = { x.options with animation = newNested } }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.crosshair(?color:string,?opacity:float,?orientation:string,?trigger:string) =
    let o = x.options.crosshair
    let newNested = { ChartCrosshair.color = right o "color" color; opacity = right o "opacity" opacity; orientation = right o "orientation" orientation; trigger = right o "trigger" trigger; focused = copy o "focused"; selected = copy o "selected" }
    { x with options = { x.options with crosshair = newNested } }
  member x.explorer(?actions:seq<string>,?axis:string,?keepInBounds:bool,?maxZoomIn:float,?maxZoomOut:float,?zoomDelta:float) =
    let o = x.options.explorer
    let newNested = { ChartExplorer.actions = right o "actions" (Option.map Array.ofSeq actions); axis = right o "axis" axis; keepInBounds = right o "keepInBounds" keepInBounds; maxZoomIn = right o "maxZoomIn" maxZoomIn; maxZoomOut = right o "maxZoomOut" maxZoomOut; zoomDelta = right o "zoomDelta" zoomDelta;  }
    { x with options = { x.options with explorer = newNested } }
  member x.hAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.hAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with hAxis = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
  member x.vAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.vAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with vAxis = newNested } }
type Bar = 
  { data : ChartData; typeName : string; 
    options : BarChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?aggregationTarget:string,?axisTitlesPosition:string,?backgroundColor:obj,?colors:seq<string>,?dataOpacity:float,?enableInteractivity:bool,?focusTarget:string,?fontSize:float,?fontName:string,?hAxes:obj,?height:float,?isStacked:bool,?reverseCategories:bool,?series:obj,?theme:string,?title:string,?titlePosition:string,?vAxes:obj,?width:float) = 
    let o = x.options
    let newOptions = { x.options with aggregationTarget = right o "aggregationTarget" aggregationTarget; axisTitlesPosition = right o "axisTitlesPosition" axisTitlesPosition; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); dataOpacity = right o "dataOpacity" dataOpacity; enableInteractivity = right o "enableInteractivity" enableInteractivity; focusTarget = right o "focusTarget" focusTarget; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; hAxes = right o "hAxes" hAxes; height = right o "height" height; isStacked = right o "isStacked" isStacked; reverseCategories = right o "reverseCategories" reverseCategories; series = right o "series" series; theme = right o "theme" theme; title = right o "title" title; titlePosition = right o "titlePosition" titlePosition; vAxes = right o "vAxes" vAxes; width = right o "width" width }
    { x with options = newOptions }
  member x.animation(?duration:float,?easing:string) =
    let o = x.options.animation
    let newNested = { TransitionAnimation.duration = right o "duration" duration; easing = right o "easing" easing;  }
    { x with options = { x.options with animation = newNested } }
  member x.bar(?groupWidth:obj) =
    let o = x.options.bar
    let newNested = { GroupWidth.groupWidth = right o "groupWidth" groupWidth;  }
    { x with options = { x.options with bar = newNested } }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.hAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.hAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with hAxis = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
  member x.vAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.vAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with vAxis = newNested } }
type Histogram = 
  { data : ChartData; typeName : string; 
    options : HistogramOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?axisTitlesPosition:string,?backgroundColor:obj,?colors:seq<string>,?dataOpacity:float,?enableInteractivity:bool,?focusTarget:string,?fontSize:float,?fontName:string,?height:float,?interpolateNulls:bool,?isStacked:bool,?orientation:string,?reverseCategories:bool,?series:obj,?theme:string,?title:string,?titlePosition:string,?vAxes:obj,?width:float) = 
    let o = x.options
    let newOptions = { x.options with axisTitlesPosition = right o "axisTitlesPosition" axisTitlesPosition; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); dataOpacity = right o "dataOpacity" dataOpacity; enableInteractivity = right o "enableInteractivity" enableInteractivity; focusTarget = right o "focusTarget" focusTarget; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; height = right o "height" height; interpolateNulls = right o "interpolateNulls" interpolateNulls; isStacked = right o "isStacked" isStacked; orientation = right o "orientation" orientation; reverseCategories = right o "reverseCategories" reverseCategories; series = right o "series" series; theme = right o "theme" theme; title = right o "title" title; titlePosition = right o "titlePosition" titlePosition; vAxes = right o "vAxes" vAxes; width = right o "width" width }
    { x with options = newOptions }
  member x.animation(?duration:float,?easing:string) =
    let o = x.options.animation
    let newNested = { TransitionAnimation.duration = right o "duration" duration; easing = right o "easing" easing;  }
    { x with options = { x.options with animation = newNested } }
  member x.bar(?groupWidth:obj) =
    let o = x.options.bar
    let newNested = { GroupWidth.groupWidth = right o "groupWidth" groupWidth;  }
    { x with options = { x.options with bar = newNested } }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.hAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.hAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with hAxis = newNested } }
  member x.histogram(?bucketSize:float,?hideBucketItems:bool,?lastBucketPercentile:float) =
    let o = x.options.histogram
    let newNested = { HistogramHistogram.bucketSize = right o "bucketSize" bucketSize; hideBucketItems = right o "hideBucketItems" hideBucketItems; lastBucketPercentile = right o "lastBucketPercentile" lastBucketPercentile;  }
    { x with options = { x.options with histogram = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
  member x.vAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.vAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with vAxis = newNested } }
type Area = 
  { data : ChartData; typeName : string; 
    options : AreaChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?aggregationTarget:string,?areaOpacity:float,?axisTitlesPosition:string,?backgroundColor:obj,?colors:seq<string>,?dataOpacity:float,?enableInteractivity:bool,?focusTarget:string,?fontSize:float,?fontName:string,?height:float,?interpolateNulls:bool,?isStacked:bool,?lineWidth:float,?orientation:string,?pointSize:float,?reverseCategories:bool,?selectionMode:string,?series:obj,?theme:string,?title:string,?titlePosition:string,?vAxes:obj,?width:float) = 
    let o = x.options
    let newOptions = { x.options with aggregationTarget = right o "aggregationTarget" aggregationTarget; areaOpacity = right o "areaOpacity" areaOpacity; axisTitlesPosition = right o "axisTitlesPosition" axisTitlesPosition; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); dataOpacity = right o "dataOpacity" dataOpacity; enableInteractivity = right o "enableInteractivity" enableInteractivity; focusTarget = right o "focusTarget" focusTarget; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; height = right o "height" height; interpolateNulls = right o "interpolateNulls" interpolateNulls; isStacked = right o "isStacked" isStacked; lineWidth = right o "lineWidth" lineWidth; orientation = right o "orientation" orientation; pointSize = right o "pointSize" pointSize; reverseCategories = right o "reverseCategories" reverseCategories; selectionMode = right o "selectionMode" selectionMode; series = right o "series" series; theme = right o "theme" theme; title = right o "title" title; titlePosition = right o "titlePosition" titlePosition; vAxes = right o "vAxes" vAxes; width = right o "width" width }
    { x with options = newOptions }
  member x.animation(?duration:float,?easing:string) =
    let o = x.options.animation
    let newNested = { TransitionAnimation.duration = right o "duration" duration; easing = right o "easing" easing;  }
    { x with options = { x.options with animation = newNested } }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.crosshair(?color:string,?opacity:float,?orientation:string,?trigger:string) =
    let o = x.options.crosshair
    let newNested = { ChartCrosshair.color = right o "color" color; opacity = right o "opacity" opacity; orientation = right o "orientation" orientation; trigger = right o "trigger" trigger; focused = copy o "focused"; selected = copy o "selected" }
    { x with options = { x.options with crosshair = newNested } }
  member x.explorer(?actions:seq<string>,?axis:string,?keepInBounds:bool,?maxZoomIn:float,?maxZoomOut:float,?zoomDelta:float) =
    let o = x.options.explorer
    let newNested = { ChartExplorer.actions = right o "actions" (Option.map Array.ofSeq actions); axis = right o "axis" axis; keepInBounds = right o "keepInBounds" keepInBounds; maxZoomIn = right o "maxZoomIn" maxZoomIn; maxZoomOut = right o "maxZoomOut" maxZoomOut; zoomDelta = right o "zoomDelta" zoomDelta;  }
    { x with options = { x.options with explorer = newNested } }
  member x.hAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.hAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with hAxis = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
  member x.vAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.vAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with vAxis = newNested } }
type Annotation = 
  { data : ChartData; typeName : string; 
    options : AnnotationChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?allowHtml:bool,?allValuesSuffix:string,?annotationsWidth:float,?colors:seq<string>,?dateFormat:string,?displayAnnotations:bool,?displayAnnotationsFilter:bool,?displayDateTimeBarSeparator:bool,?displayExactValues:bool,?displayLegendDots:bool,?displayLegendValues:bool,?displayRangeSelector:bool,?displayZoomButtons:bool,?fill:float,?legendPosition:string,?max:float,?min:float,?numberFormats:obj,?scaleColumns:seq<float>,?scaleFormat:string,?scaleType:string,?thickness:float,?zoomEndTime:DateTime,?zoomStartTime:DateTime) = 
    let o = x.options
    let newOptions = { x.options with allowHtml = right o "allowHtml" allowHtml; allValuesSuffix = right o "allValuesSuffix" allValuesSuffix; annotationsWidth = right o "annotationsWidth" annotationsWidth; colors = right o "colors" (Option.map Array.ofSeq colors); dateFormat = right o "dateFormat" dateFormat; displayAnnotations = right o "displayAnnotations" displayAnnotations; displayAnnotationsFilter = right o "displayAnnotationsFilter" displayAnnotationsFilter; displayDateTimeBarSeparator = right o "displayDateTimeBarSeparator" displayDateTimeBarSeparator; displayExactValues = right o "displayExactValues" displayExactValues; displayLegendDots = right o "displayLegendDots" displayLegendDots; displayLegendValues = right o "displayLegendValues" displayLegendValues; displayRangeSelector = right o "displayRangeSelector" displayRangeSelector; displayZoomButtons = right o "displayZoomButtons" displayZoomButtons; fill = right o "fill" fill; legendPosition = right o "legendPosition" legendPosition; max = right o "max" max; min = right o "min" min; numberFormats = right o "numberFormats" numberFormats; scaleColumns = right o "scaleColumns" (Option.map Array.ofSeq scaleColumns); scaleFormat = right o "scaleFormat" scaleFormat; scaleType = right o "scaleType" scaleType; thickness = right o "thickness" thickness; zoomEndTime = right o "zoomEndTime" zoomEndTime; zoomStartTime = right o "zoomStartTime" zoomStartTime }
    { x with options = newOptions }
type SteppedArea = 
  { data : ChartData; typeName : string; 
    options : SteppedAreaChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?aggregationTarget:string,?areaOpacity:float,?axisTitlesPosition:string,?backgroundColor:obj,?colors:seq<string>,?connectSteps:bool,?enableInteractivity:bool,?focusTarget:string,?fontSize:float,?fontName:string,?height:float,?interpolateNulls:bool,?isStacked:bool,?reverseCategories:bool,?selectionMode:string,?series:obj,?theme:string,?title:string,?titlePosition:string,?vAxes:obj,?width:float) = 
    let o = x.options
    let newOptions = { x.options with aggregationTarget = right o "aggregationTarget" aggregationTarget; areaOpacity = right o "areaOpacity" areaOpacity; axisTitlesPosition = right o "axisTitlesPosition" axisTitlesPosition; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); connectSteps = right o "connectSteps" connectSteps; enableInteractivity = right o "enableInteractivity" enableInteractivity; focusTarget = right o "focusTarget" focusTarget; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; height = right o "height" height; interpolateNulls = right o "interpolateNulls" interpolateNulls; isStacked = right o "isStacked" isStacked; reverseCategories = right o "reverseCategories" reverseCategories; selectionMode = right o "selectionMode" selectionMode; series = right o "series" series; theme = right o "theme" theme; title = right o "title" title; titlePosition = right o "titlePosition" titlePosition; vAxes = right o "vAxes" vAxes; width = right o "width" width }
    { x with options = newOptions }
  member x.animation(?duration:float,?easing:string) =
    let o = x.options.animation
    let newNested = { TransitionAnimation.duration = right o "duration" duration; easing = right o "easing" easing;  }
    { x with options = { x.options with animation = newNested } }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.hAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.hAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with hAxis = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
  member x.vAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.vAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with vAxis = newNested } }
type Pie = 
  { data : ChartData; typeName : string; 
    options : PieChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?backgroundColor:obj,?colors:seq<string>,?enableInteractivity:bool,?fontSize:float,?fontName:string,?height:float,?is3D:bool,?pieHole:float,?pieSliceBorderColor:string,?pieSliceText:string,?pieStartAngle:float,?reverseCategories:bool,?pieResidueSliceColor:string,?pieResidueSliceLabel:string,?slices:obj,?sliceVisibilityThreshold:float,?title:string,?width:float) = 
    let o = x.options
    let newOptions = { x.options with backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); enableInteractivity = right o "enableInteractivity" enableInteractivity; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; height = right o "height" height; is3D = right o "is3D" is3D; pieHole = right o "pieHole" pieHole; pieSliceBorderColor = right o "pieSliceBorderColor" pieSliceBorderColor; pieSliceText = right o "pieSliceText" pieSliceText; pieStartAngle = right o "pieStartAngle" pieStartAngle; reverseCategories = right o "reverseCategories" reverseCategories; pieResidueSliceColor = right o "pieResidueSliceColor" pieResidueSliceColor; pieResidueSliceLabel = right o "pieResidueSliceLabel" pieResidueSliceLabel; slices = right o "slices" slices; sliceVisibilityThreshold = right o "sliceVisibilityThreshold" sliceVisibilityThreshold; title = right o "title" title; width = right o "width" width }
    { x with options = newOptions }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.pieSliceTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.pieSliceTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with pieSliceTextStyle = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
type Bubble = 
  { data : ChartData; typeName : string; 
    options : BubbleChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?axisTitlesPosition:string,?backgroundColor:obj,?colors:seq<string>,?enableInteractivity:bool,?fontSize:float,?fontName:string,?forceIFrame:bool,?height:float,?selectionMode:string,?series:obj,?sortBubblesBySize:bool,?theme:string,?title:string,?titlePosition:string,?width:float) = 
    let o = x.options
    let newOptions = { x.options with axisTitlesPosition = right o "axisTitlesPosition" axisTitlesPosition; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); enableInteractivity = right o "enableInteractivity" enableInteractivity; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; forceIFrame = right o "forceIFrame" forceIFrame; height = right o "height" height; selectionMode = right o "selectionMode" selectionMode; series = right o "series" series; sortBubblesBySize = right o "sortBubblesBySize" sortBubblesBySize; theme = right o "theme" theme; title = right o "title" title; titlePosition = right o "titlePosition" titlePosition; width = right o "width" width }
    { x with options = newOptions }
  member x.animation(?duration:float,?easing:string) =
    let o = x.options.animation
    let newNested = { TransitionAnimation.duration = right o "duration" duration; easing = right o "easing" easing;  }
    { x with options = { x.options with animation = newNested } }
  member x.bubble(?opacity:float,?stroke:string) =
    let o = x.options.bubble
    let newNested = { ChartBubble.opacity = right o "opacity" opacity; stroke = right o "stroke" stroke; textStyle = copy o "textStyle" }
    { x with options = { x.options with bubble = newNested } }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.colorAxis(?minValue:float,?maxValue:float,?values:seq<float>,?colors:seq<string>) =
    let o = x.options.colorAxis
    let newNested = { ChartColorAxis.minValue = right o "minValue" minValue; maxValue = right o "maxValue" maxValue; values = right o "values" (Option.map Array.ofSeq values); colors = right o "colors" (Option.map Array.ofSeq colors); legend = copy o "legend" }
    { x with options = { x.options with colorAxis = newNested } }
  member x.explorer(?actions:seq<string>,?axis:string,?keepInBounds:bool,?maxZoomIn:float,?maxZoomOut:float,?zoomDelta:float) =
    let o = x.options.explorer
    let newNested = { ChartExplorer.actions = right o "actions" (Option.map Array.ofSeq actions); axis = right o "axis" axis; keepInBounds = right o "keepInBounds" keepInBounds; maxZoomIn = right o "maxZoomIn" maxZoomIn; maxZoomOut = right o "maxZoomOut" maxZoomOut; zoomDelta = right o "zoomDelta" zoomDelta;  }
    { x with options = { x.options with explorer = newNested } }
  member x.hAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.hAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with hAxis = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.sizeAxis(?maxSize:float,?maxValue:float,?minSize:float,?minValue:float) =
    let o = x.options.sizeAxis
    let newNested = { ChartSizeAxis.maxSize = right o "maxSize" maxSize; maxValue = right o "maxValue" maxValue; minSize = right o "minSize" minSize; minValue = right o "minValue" minValue;  }
    { x with options = { x.options with sizeAxis = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
  member x.vAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.vAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with vAxis = newNested } }
type TreeMap = 
  { data : ChartData; typeName : string; 
    options : TreeMapOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?fontColor:string,?fontFamily:string,?fontSize:float,?forceIFrame:bool,?headerColor:string,?headerHeight:float,?headerHighlightColor:string,?hintOpacity:float,?maxColor:string,?maxDepth:float,?maxHighlightColor:string,?maxPostDepth:float,?maxColorValue:float,?midColor:string,?midHighlightColor:string,?minColor:string,?minHighlightColor:string,?minColorValue:float,?showScale:bool,?showTooltips:bool,?title:string,?useWeightedAverageForAggregation:bool) = 
    let o = x.options
    let newOptions = { x.options with fontColor = right o "fontColor" fontColor; fontFamily = right o "fontFamily" fontFamily; fontSize = right o "fontSize" fontSize; forceIFrame = right o "forceIFrame" forceIFrame; headerColor = right o "headerColor" headerColor; headerHeight = right o "headerHeight" headerHeight; headerHighlightColor = right o "headerHighlightColor" headerHighlightColor; hintOpacity = right o "hintOpacity" hintOpacity; maxColor = right o "maxColor" maxColor; maxDepth = right o "maxDepth" maxDepth; maxHighlightColor = right o "maxHighlightColor" maxHighlightColor; maxPostDepth = right o "maxPostDepth" maxPostDepth; maxColorValue = right o "maxColorValue" maxColorValue; midColor = right o "midColor" midColor; midHighlightColor = right o "midHighlightColor" midHighlightColor; minColor = right o "minColor" minColor; minHighlightColor = right o "minHighlightColor" minHighlightColor; minColorValue = right o "minColorValue" minColorValue; showScale = right o "showScale" showScale; showTooltips = right o "showTooltips" showTooltips; title = right o "title" title; useWeightedAverageForAggregation = right o "useWeightedAverageForAggregation" useWeightedAverageForAggregation }
    { x with options = newOptions }
  member x.textStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.textStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with textStyle = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
type Table = 
  { data : ChartData; typeName : string; 
    options : TableOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?allowHtml:bool,?alternatingRowStyle:bool,?firstRowNumber:float,?height:string,?page:string,?pageSize:float,?rtlTable:bool,?scrollLeftStartPosition:float,?showRowNumber:bool,?sort:string,?sortAscending:bool,?sortColumn:float,?startPage:float,?width:string) = 
    let o = x.options
    let newOptions = { x.options with allowHtml = right o "allowHtml" allowHtml; alternatingRowStyle = right o "alternatingRowStyle" alternatingRowStyle; firstRowNumber = right o "firstRowNumber" firstRowNumber; height = right o "height" height; page = right o "page" page; pageSize = right o "pageSize" pageSize; rtlTable = right o "rtlTable" rtlTable; scrollLeftStartPosition = right o "scrollLeftStartPosition" scrollLeftStartPosition; showRowNumber = right o "showRowNumber" showRowNumber; sort = right o "sort" sort; sortAscending = right o "sortAscending" sortAscending; sortColumn = right o "sortColumn" sortColumn; startPage = right o "startPage" startPage; width = right o "width" width }
    { x with options = newOptions }
  member x.cssClassName(?headerRow:string,?tableRow:string,?oddTableRow:string,?selectedTableRow:string,?hoverTableRow:string,?headerCell:string,?tableCell:string,?rowNumberCell:string) =
    let o = x.options.cssClassName
    let newNested = { CssClassNames.headerRow = right o "headerRow" headerRow; tableRow = right o "tableRow" tableRow; oddTableRow = right o "oddTableRow" oddTableRow; selectedTableRow = right o "selectedTableRow" selectedTableRow; hoverTableRow = right o "hoverTableRow" hoverTableRow; headerCell = right o "headerCell" headerCell; tableCell = right o "tableCell" tableCell; rowNumberCell = right o "rowNumberCell" rowNumberCell;  }
    { x with options = { x.options with cssClassName = newNested } }
type Timeline = 
  { data : ChartData; typeName : string; 
    options : TimelineOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?avoidOverlappingGridLines:bool,?backgroundColor:obj,?colors:seq<string>,?enableInteractivity:bool,?forceIFrame:bool,?height:float,?width:float) = 
    let o = x.options
    let newOptions = { x.options with avoidOverlappingGridLines = right o "avoidOverlappingGridLines" avoidOverlappingGridLines; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); enableInteractivity = right o "enableInteractivity" enableInteractivity; forceIFrame = right o "forceIFrame" forceIFrame; height = right o "height" height; width = right o "width" width }
    { x with options = newOptions }
  member x.timeline(?colorByRowLabel:bool,?groupByRowLabel:bool,?showRowLabels:bool,?singleColor:string) =
    let o = x.options.timeline
    let newNested = { TimelineTimeline.colorByRowLabel = right o "colorByRowLabel" colorByRowLabel; groupByRowLabel = right o "groupByRowLabel" groupByRowLabel; showRowLabels = right o "showRowLabels" showRowLabels; singleColor = right o "singleColor" singleColor; barLabelStyle = copy o "barLabelStyle"; rowLabelStyle = copy o "rowLabelStyle" }
    { x with options = { x.options with timeline = newNested } }
type Candlestick = 
  { data : ChartData; typeName : string; 
    options : CandlestickChartOptions }
  interface Chart
  member x.show(outputId) = Helpers.showChart x outputId
  member x.set(?aggregationTarget:string,?axisTitlesPosition:string,?backgroundColor:obj,?colors:seq<string>,?enableInteractivity:bool,?focusTarget:string,?fontSize:float,?fontName:string,?height:float,?orientation:string,?reverseCategories:bool,?selectionMode:string,?series:obj,?theme:string,?title:string,?titlePosition:string,?vAxes:obj,?width:float) = 
    let o = x.options
    let newOptions = { x.options with aggregationTarget = right o "aggregationTarget" aggregationTarget; axisTitlesPosition = right o "axisTitlesPosition" axisTitlesPosition; backgroundColor = right o "backgroundColor" backgroundColor; colors = right o "colors" (Option.map Array.ofSeq colors); enableInteractivity = right o "enableInteractivity" enableInteractivity; focusTarget = right o "focusTarget" focusTarget; fontSize = right o "fontSize" fontSize; fontName = right o "fontName" fontName; height = right o "height" height; orientation = right o "orientation" orientation; reverseCategories = right o "reverseCategories" reverseCategories; selectionMode = right o "selectionMode" selectionMode; series = right o "series" series; theme = right o "theme" theme; title = right o "title" title; titlePosition = right o "titlePosition" titlePosition; vAxes = right o "vAxes" vAxes; width = right o "width" width }
    { x with options = newOptions }
  member x.animation(?duration:float,?easing:string) =
    let o = x.options.animation
    let newNested = { TransitionAnimation.duration = right o "duration" duration; easing = right o "easing" easing;  }
    { x with options = { x.options with animation = newNested } }
  member x.bar(?groupWidth:obj) =
    let o = x.options.bar
    let newNested = { GroupWidth.groupWidth = right o "groupWidth" groupWidth;  }
    { x with options = { x.options with bar = newNested } }
  member x.candlestick(?hollowIsRising:bool) =
    let o = x.options.candlestick
    let newNested = { CandlestickCandlestick.hollowIsRising = right o "hollowIsRising" hollowIsRising; fallingColor = copy o "fallingColor"; risingColor = copy o "risingColor" }
    { x with options = { x.options with candlestick = newNested } }
  member x.chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    let o = x.options.chartArea
    let newNested = { ChartArea.top = right o "top" top; left = right o "left" left; width = right o "width" width; height = right o "height" height;  }
    { x with options = { x.options with chartArea = newNested } }
  member x.hAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.hAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with hAxis = newNested } }
  member x.legend(?alignment:string,?maxLines:float,?position:string,?numberFormat:string) =
    let o = x.options.legend
    let newNested = { ChartLegend.alignment = right o "alignment" alignment; maxLines = right o "maxLines" maxLines; position = right o "position" position; numberFormat = right o "numberFormat" numberFormat; textStyle = copy o "textStyle" }
    { x with options = { x.options with legend = newNested } }
  member x.titleTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    let o = x.options.titleTextStyle
    let newNested = { ChartTextStyle.fontName = right o "fontName" fontName; fontSize = right o "fontSize" fontSize; bold = right o "bold" bold; italic = right o "italic" italic; color = right o "color" color; auraColor = right o "auraColor" auraColor; opacity = right o "opacity" opacity;  }
    { x with options = { x.options with titleTextStyle = newNested } }
  member x.tooltip(?isHtml:bool,?showColorCode:bool,?trigger:string) =
    let o = x.options.tooltip
    let newNested = { ChartTooltip.isHtml = right o "isHtml" isHtml; showColorCode = right o "showColorCode" showColorCode; trigger = right o "trigger" trigger; textStyle = copy o "textStyle" }
    { x with options = { x.options with tooltip = newNested } }
  member x.vAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?logScale:bool,?textPosition:string,?ticks:seq<obj>,?title:string,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string) =
    let o = x.options.vAxis
    let newNested = { ChartAxis.baseline = right o "baseline" baseline; baselineColor = right o "baselineColor" baselineColor; direction = right o "direction" direction; format = right o "format" format; logScale = right o "logScale" logScale; textPosition = right o "textPosition" textPosition; ticks = right o "ticks" (Option.map Array.ofSeq ticks); title = right o "title" title; allowContainerBoundaryTextCufoff = right o "allowContainerBoundaryTextCufoff" allowContainerBoundaryTextCufoff; slantedText = right o "slantedText" slantedText; slantedTextAngle = right o "slantedTextAngle" slantedTextAngle; maxAlternation = right o "maxAlternation" maxAlternation; maxTextLines = right o "maxTextLines" maxTextLines; minTextSpacing = right o "minTextSpacing" minTextSpacing; showTextEvery = right o "showTextEvery" showTextEvery; maxValue = right o "maxValue" maxValue; minValue = right o "minValue" minValue; viewWindowMode = right o "viewWindowMode" viewWindowMode; gridlines = copy o "gridlines"; minorGridlines = copy o "minorGridlines"; textStyle = copy o "textStyle"; titleTextStyle = copy o "titleTextStyle"; viewWindow = copy o "viewWindow" }
    { x with options = { x.options with vAxis = newNested } }

type GeoChartOptions with
  static member empty =
    { GeoChartOptions.backgroundColor = undefined<_>(); colorAxis = undefined<_>(); datalessRegionColor = undefined<_>(); displayMode = undefined<_>(); enableRegionInteractivity = undefined<_>(); height = undefined<_>(); keepAspectRatio = undefined<_>(); legend = undefined<_>(); region = undefined<_>(); magnifyingGlass = undefined<_>(); markerOpacity = undefined<_>(); resolution = undefined<_>(); sizeAxis = undefined<_>(); tooltip = undefined<_>(); width = undefined<_>() }
type ScatterChartOptions with
  static member empty =
    { ScatterChartOptions.aggregationTarget = undefined<_>(); animation = undefined<_>(); annotations = undefined<_>(); axisTitlesPosition = undefined<_>(); backgroundColor = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); crosshair = undefined<_>(); curveType = undefined<_>(); dataOpacity = undefined<_>(); enableInteractivity = undefined<_>(); explorer = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); forceIFrame = undefined<_>(); hAxis = undefined<_>(); height = undefined<_>(); legend = undefined<_>(); lineWidth = undefined<_>(); pointSize = undefined<_>(); selectionMode = undefined<_>(); series = undefined<_>(); theme = undefined<_>(); title = undefined<_>(); titlePosition = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); trendlines = undefined<_>(); vAxis = undefined<_>(); width = undefined<_>() }
type ColumnChartOptions with
  static member empty =
    { ColumnChartOptions.aggregationTarget = undefined<_>(); animation = undefined<_>(); annotations = undefined<_>(); axisTitlesPosition = undefined<_>(); backgroundColor = undefined<_>(); bar = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); enableInteractivity = undefined<_>(); focusTarget = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); hAxis = undefined<_>(); height = undefined<_>(); isStacked = undefined<_>(); legend = undefined<_>(); reverseCategories = undefined<_>(); selectionMode = undefined<_>(); series = undefined<_>(); theme = undefined<_>(); title = undefined<_>(); titlePosition = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); vAxes = undefined<_>(); vAxis = undefined<_>(); width = undefined<_>() }
type LineChartOptions with
  static member empty =
    { LineChartOptions.aggregationTarget = undefined<_>(); animation = undefined<_>(); annotations = undefined<_>(); axisTitlesPosition = undefined<_>(); backgroundColor = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); crosshair = undefined<_>(); curveType = undefined<_>(); dataOpacity = undefined<_>(); enableInteractivity = undefined<_>(); explorer = undefined<_>(); focusTarget = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); hAxis = undefined<_>(); height = undefined<_>(); interpolateNulls = undefined<_>(); legend = undefined<_>(); lineWidth = undefined<_>(); orientation = undefined<_>(); pointSize = undefined<_>(); reverseCategories = undefined<_>(); selectionMode = undefined<_>(); series = undefined<_>(); theme = undefined<_>(); title = undefined<_>(); titlePosition = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); vAxes = undefined<_>(); vAxis = undefined<_>(); width = undefined<_>() }
type BarChartOptions with
  static member empty =
    { BarChartOptions.aggregationTarget = undefined<_>(); animation = undefined<_>(); annotations = undefined<_>(); axisTitlesPosition = undefined<_>(); backgroundColor = undefined<_>(); bar = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); dataOpacity = undefined<_>(); enableInteractivity = undefined<_>(); focusTarget = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); hAxes = undefined<_>(); hAxis = undefined<_>(); height = undefined<_>(); isStacked = undefined<_>(); legend = undefined<_>(); reverseCategories = undefined<_>(); series = undefined<_>(); theme = undefined<_>(); title = undefined<_>(); titlePosition = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); vAxes = undefined<_>(); vAxis = undefined<_>(); width = undefined<_>() }
type HistogramOptions with
  static member empty =
    { HistogramOptions.animation = undefined<_>(); axisTitlesPosition = undefined<_>(); backgroundColor = undefined<_>(); bar = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); dataOpacity = undefined<_>(); enableInteractivity = undefined<_>(); focusTarget = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); hAxis = undefined<_>(); histogram = undefined<_>(); height = undefined<_>(); interpolateNulls = undefined<_>(); isStacked = undefined<_>(); legend = undefined<_>(); orientation = undefined<_>(); reverseCategories = undefined<_>(); series = undefined<_>(); theme = undefined<_>(); title = undefined<_>(); titlePosition = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); vAxes = undefined<_>(); vAxis = undefined<_>(); width = undefined<_>() }
type AreaChartOptions with
  static member empty =
    { AreaChartOptions.aggregationTarget = undefined<_>(); animation = undefined<_>(); areaOpacity = undefined<_>(); axisTitlesPosition = undefined<_>(); backgroundColor = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); crosshair = undefined<_>(); dataOpacity = undefined<_>(); enableInteractivity = undefined<_>(); explorer = undefined<_>(); focusTarget = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); hAxis = undefined<_>(); height = undefined<_>(); interpolateNulls = undefined<_>(); isStacked = undefined<_>(); legend = undefined<_>(); lineWidth = undefined<_>(); orientation = undefined<_>(); pointSize = undefined<_>(); reverseCategories = undefined<_>(); selectionMode = undefined<_>(); series = undefined<_>(); theme = undefined<_>(); title = undefined<_>(); titlePosition = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); vAxes = undefined<_>(); vAxis = undefined<_>(); width = undefined<_>() }
type AnnotationChartOptions with
  static member empty =
    { AnnotationChartOptions.allowHtml = undefined<_>(); allValuesSuffix = undefined<_>(); annotationsWidth = undefined<_>(); colors = undefined<_>(); dateFormat = undefined<_>(); displayAnnotations = undefined<_>(); displayAnnotationsFilter = undefined<_>(); displayDateTimeBarSeparator = undefined<_>(); displayExactValues = undefined<_>(); displayLegendDots = undefined<_>(); displayLegendValues = undefined<_>(); displayRangeSelector = undefined<_>(); displayZoomButtons = undefined<_>(); fill = undefined<_>(); legendPosition = undefined<_>(); max = undefined<_>(); min = undefined<_>(); numberFormats = undefined<_>(); scaleColumns = undefined<_>(); scaleFormat = undefined<_>(); scaleType = undefined<_>(); thickness = undefined<_>(); zoomEndTime = undefined<_>(); zoomStartTime = undefined<_>() }
type SteppedAreaChartOptions with
  static member empty =
    { SteppedAreaChartOptions.aggregationTarget = undefined<_>(); animation = undefined<_>(); areaOpacity = undefined<_>(); axisTitlesPosition = undefined<_>(); backgroundColor = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); connectSteps = undefined<_>(); enableInteractivity = undefined<_>(); focusTarget = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); hAxis = undefined<_>(); height = undefined<_>(); interpolateNulls = undefined<_>(); isStacked = undefined<_>(); legend = undefined<_>(); reverseCategories = undefined<_>(); selectionMode = undefined<_>(); series = undefined<_>(); theme = undefined<_>(); title = undefined<_>(); titlePosition = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); vAxes = undefined<_>(); vAxis = undefined<_>(); width = undefined<_>() }
type PieChartOptions with
  static member empty =
    { PieChartOptions.backgroundColor = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); enableInteractivity = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); height = undefined<_>(); is3D = undefined<_>(); legend = undefined<_>(); pieHole = undefined<_>(); pieSliceBorderColor = undefined<_>(); pieSliceText = undefined<_>(); pieSliceTextStyle = undefined<_>(); pieStartAngle = undefined<_>(); reverseCategories = undefined<_>(); pieResidueSliceColor = undefined<_>(); pieResidueSliceLabel = undefined<_>(); slices = undefined<_>(); sliceVisibilityThreshold = undefined<_>(); title = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); width = undefined<_>() }
type BubbleChartOptions with
  static member empty =
    { BubbleChartOptions.animation = undefined<_>(); axisTitlesPosition = undefined<_>(); backgroundColor = undefined<_>(); bubble = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); colorAxis = undefined<_>(); enableInteractivity = undefined<_>(); explorer = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); forceIFrame = undefined<_>(); hAxis = undefined<_>(); height = undefined<_>(); legend = undefined<_>(); selectionMode = undefined<_>(); series = undefined<_>(); sizeAxis = undefined<_>(); sortBubblesBySize = undefined<_>(); theme = undefined<_>(); title = undefined<_>(); titlePosition = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); vAxis = undefined<_>(); width = undefined<_>() }
type TreeMapOptions with
  static member empty =
    { TreeMapOptions.fontColor = undefined<_>(); fontFamily = undefined<_>(); fontSize = undefined<_>(); forceIFrame = undefined<_>(); headerColor = undefined<_>(); headerHeight = undefined<_>(); headerHighlightColor = undefined<_>(); hintOpacity = undefined<_>(); maxColor = undefined<_>(); maxDepth = undefined<_>(); maxHighlightColor = undefined<_>(); maxPostDepth = undefined<_>(); maxColorValue = undefined<_>(); midColor = undefined<_>(); midHighlightColor = undefined<_>(); minColor = undefined<_>(); minHighlightColor = undefined<_>(); minColorValue = undefined<_>(); showScale = undefined<_>(); showTooltips = undefined<_>(); textStyle = undefined<_>(); title = undefined<_>(); titleTextStyle = undefined<_>(); useWeightedAverageForAggregation = undefined<_>() }
type TableOptions with
  static member empty =
    { TableOptions.allowHtml = undefined<_>(); alternatingRowStyle = undefined<_>(); cssClassName = undefined<_>(); firstRowNumber = undefined<_>(); height = undefined<_>(); page = undefined<_>(); pageSize = undefined<_>(); rtlTable = undefined<_>(); scrollLeftStartPosition = undefined<_>(); showRowNumber = undefined<_>(); sort = undefined<_>(); sortAscending = undefined<_>(); sortColumn = undefined<_>(); startPage = undefined<_>(); width = undefined<_>() }
type TimelineOptions with
  static member empty =
    { TimelineOptions.avoidOverlappingGridLines = undefined<_>(); backgroundColor = undefined<_>(); colors = undefined<_>(); enableInteractivity = undefined<_>(); forceIFrame = undefined<_>(); height = undefined<_>(); timeline = undefined<_>(); width = undefined<_>() }
type CandlestickChartOptions with
  static member empty =
    { CandlestickChartOptions.aggregationTarget = undefined<_>(); animation = undefined<_>(); axisTitlesPosition = undefined<_>(); backgroundColor = undefined<_>(); bar = undefined<_>(); candlestick = undefined<_>(); chartArea = undefined<_>(); colors = undefined<_>(); enableInteractivity = undefined<_>(); focusTarget = undefined<_>(); fontSize = undefined<_>(); fontName = undefined<_>(); hAxis = undefined<_>(); height = undefined<_>(); legend = undefined<_>(); orientation = undefined<_>(); reverseCategories = undefined<_>(); selectionMode = undefined<_>(); series = undefined<_>(); theme = undefined<_>(); title = undefined<_>(); titlePosition = undefined<_>(); titleTextStyle = undefined<_>(); tooltip = undefined<_>(); vAxes = undefined<_>(); vAxis = undefined<_>(); width = undefined<_>() }

type options =
  static member chartSizeAxis(?maxSize:float,?maxValue:float,?minSize:float,?minValue:float) =
    { ChartSizeAxis.maxSize = orDefault maxSize; maxValue = orDefault maxValue; minSize = orDefault minSize; minValue = orDefault minValue }
  static member chartTextStyle(?fontName:string,?fontSize:float,?bold:bool,?italic:bool,?color:string,?auraColor:string,?opacity:float) =
    { ChartTextStyle.fontName = orDefault fontName; fontSize = orDefault fontSize; bold = orDefault bold; italic = orDefault italic; color = orDefault color; auraColor = orDefault auraColor; opacity = orDefault opacity }
  static member chartTooltip(?isHtml:bool,?showColorCode:bool,?textStyle:ChartTextStyle,?trigger:string) =
    { ChartTooltip.isHtml = orDefault isHtml; showColorCode = orDefault showColorCode; textStyle = orDefault textStyle; trigger = orDefault trigger }
  static member chartLegend(?alignment:string,?maxLines:float,?position:string,?textStyle:ChartTextStyle,?numberFormat:string) =
    { ChartLegend.alignment = orDefault alignment; maxLines = orDefault maxLines; position = orDefault position; textStyle = orDefault textStyle; numberFormat = orDefault numberFormat }
  static member chartColorAxis(?minValue:float,?maxValue:float,?values:seq<float>,?colors:seq<string>,?legend:ChartLegend) =
    { ChartColorAxis.minValue = orDefault minValue; maxValue = orDefault maxValue; values = orDefault (Option.map Array.ofSeq values); colors = orDefault (Option.map Array.ofSeq colors); legend = orDefault legend }
  static member geoChartMagnifyingGlass(?enable:bool,?zoomFactor:float) =
    { GeoChartMagnifyingGlass.enable = orDefault enable; zoomFactor = orDefault zoomFactor }
  static member chartBoxStyleGradient(?color1:string,?color2:string,?x1:string,?y1:string,?x2:string,?y2:string,?useObjectBoundingBoxUnits:bool) =
    { ChartBoxStyleGradient.color1 = orDefault color1; color2 = orDefault color2; x1 = orDefault x1; y1 = orDefault y1; x2 = orDefault x2; y2 = orDefault y2; useObjectBoundingBoxUnits = orDefault useObjectBoundingBoxUnits }
  static member chartBoxStyle(?stroke:string,?strokeWidth:float,?rx:float,?ry:float,?gradient:ChartBoxStyleGradient) =
    { ChartBoxStyle.stroke = orDefault stroke; strokeWidth = orDefault strokeWidth; rx = orDefault rx; ry = orDefault ry; gradient = orDefault gradient }
  static member chartAnnotations(?boxStyle:ChartBoxStyle,?textStyle:ChartTextStyle) =
    { ChartAnnotations.boxStyle = orDefault boxStyle; textStyle = orDefault textStyle }
  static member chartCrosshairFocused(?color:string,?opacity:float) =
    { ChartCrosshairFocused.color = orDefault color; opacity = orDefault opacity }
  static member chartCrosshairSelected(?color:string,?opacity:float) =
    { ChartCrosshairSelected.color = orDefault color; opacity = orDefault opacity }
  static member chartCrosshair(?color:string,?focused:ChartCrosshairFocused,?opacity:float,?orientation:string,?selected:ChartCrosshairSelected,?trigger:string) =
    { ChartCrosshair.color = orDefault color; focused = orDefault focused; opacity = orDefault opacity; orientation = orDefault orientation; selected = orDefault selected; trigger = orDefault trigger }
  static member chartExplorer(?actions:seq<string>,?axis:string,?keepInBounds:bool,?maxZoomIn:float,?maxZoomOut:float,?zoomDelta:float) =
    { ChartExplorer.actions = orDefault (Option.map Array.ofSeq actions); axis = orDefault axis; keepInBounds = orDefault keepInBounds; maxZoomIn = orDefault maxZoomIn; maxZoomOut = orDefault maxZoomOut; zoomDelta = orDefault zoomDelta }
  static member chartStroke(?stroke:string,?strokeWidth:float,?fill:string) =
    { ChartStroke.stroke = orDefault stroke; strokeWidth = orDefault strokeWidth; fill = orDefault fill }
  static member chartArea(?top:obj,?left:obj,?width:obj,?height:obj) =
    { ChartArea.top = orDefault top; left = orDefault left; width = orDefault width; height = orDefault height }
  static member transitionAnimation(?duration:float,?easing:string) =
    { TransitionAnimation.duration = orDefault duration; easing = orDefault easing }
  static member chartGridlines(?color:string,?count:float) =
    { ChartGridlines.color = orDefault color; count = orDefault count }
  static member chartViewWindow(?max:float,?min:float) =
    { ChartViewWindow.max = orDefault max; min = orDefault min }
  static member chartAxis(?baseline:float,?baselineColor:string,?direction:float,?format:string,?gridlines:ChartGridlines,?minorGridlines:ChartGridlines,?logScale:bool,?textPosition:string,?textStyle:ChartTextStyle,?ticks:seq<obj>,?title:string,?titleTextStyle:ChartTextStyle,?allowContainerBoundaryTextCufoff:bool,?slantedText:bool,?slantedTextAngle:float,?maxAlternation:float,?maxTextLines:float,?minTextSpacing:float,?showTextEvery:float,?maxValue:float,?minValue:float,?viewWindowMode:string,?viewWindow:ChartViewWindow) =
    { ChartAxis.baseline = orDefault baseline; baselineColor = orDefault baselineColor; direction = orDefault direction; format = orDefault format; gridlines = orDefault gridlines; minorGridlines = orDefault minorGridlines; logScale = orDefault logScale; textPosition = orDefault textPosition; textStyle = orDefault textStyle; ticks = orDefault (Option.map Array.ofSeq ticks); title = orDefault title; titleTextStyle = orDefault titleTextStyle; allowContainerBoundaryTextCufoff = orDefault allowContainerBoundaryTextCufoff; slantedText = orDefault slantedText; slantedTextAngle = orDefault slantedTextAngle; maxAlternation = orDefault maxAlternation; maxTextLines = orDefault maxTextLines; minTextSpacing = orDefault minTextSpacing; showTextEvery = orDefault showTextEvery; maxValue = orDefault maxValue; minValue = orDefault minValue; viewWindowMode = orDefault viewWindowMode; viewWindow = orDefault viewWindow }
  static member chartBoundingBox(?left:float,?top:float,?width:float,?height:float) =
    { ChartBoundingBox.left = orDefault left; top = orDefault top; width = orDefault width; height = orDefault height }
  static member groupWidth(?groupWidth:obj) =
    { GroupWidth.groupWidth = orDefault groupWidth }
  static member trendline(?color:string,?lineWidth:float,?labelInLegend:string,?opacity:float,?pointSize:float,?pointsVisible:bool,?showR2:bool,?``type``:string,?visibleInLegend:float) =
    { Trendline.color = orDefault color; lineWidth = orDefault lineWidth; labelInLegend = orDefault labelInLegend; opacity = orDefault opacity; pointSize = orDefault pointSize; pointsVisible = orDefault pointsVisible; showR2 = orDefault showR2; ``type`` = orDefault ``type``; visibleInLegend = orDefault visibleInLegend }
  static member histogramHistogram(?bucketSize:float,?hideBucketItems:bool,?lastBucketPercentile:float) =
    { HistogramHistogram.bucketSize = orDefault bucketSize; hideBucketItems = orDefault hideBucketItems; lastBucketPercentile = orDefault lastBucketPercentile }
  static member chartBubble(?opacity:float,?stroke:string,?textStyle:ChartTextStyle) =
    { ChartBubble.opacity = orDefault opacity; stroke = orDefault stroke; textStyle = orDefault textStyle }
  static member cssClassNames(?headerRow:string,?tableRow:string,?oddTableRow:string,?selectedTableRow:string,?hoverTableRow:string,?headerCell:string,?tableCell:string,?rowNumberCell:string) =
    { CssClassNames.headerRow = orDefault headerRow; tableRow = orDefault tableRow; oddTableRow = orDefault oddTableRow; selectedTableRow = orDefault selectedTableRow; hoverTableRow = orDefault hoverTableRow; headerCell = orDefault headerCell; tableCell = orDefault tableCell; rowNumberCell = orDefault rowNumberCell }
  static member labelStyle(?color:string,?fontName:string,?fontSize:string) =
    { LabelStyle.color = orDefault color; fontName = orDefault fontName; fontSize = orDefault fontSize }
  static member timelineTimeline(?barLabelStyle:LabelStyle,?colorByRowLabel:bool,?groupByRowLabel:bool,?rowLabelStyle:LabelStyle,?showRowLabels:bool,?singleColor:string) =
    { TimelineTimeline.barLabelStyle = orDefault barLabelStyle; colorByRowLabel = orDefault colorByRowLabel; groupByRowLabel = orDefault groupByRowLabel; rowLabelStyle = orDefault rowLabelStyle; showRowLabels = orDefault showRowLabels; singleColor = orDefault singleColor }
  static member candlestickCandlestick(?hollowIsRising:bool,?fallingColor:ChartStroke,?risingColor:ChartStroke) =
    { CandlestickCandlestick.hollowIsRising = orDefault hollowIsRising; fallingColor = orDefault fallingColor; risingColor = orDefault risingColor }
