// --------------------------------------------------------------------------------------------------------------------
// Google chart API 
// --------------------------------------------------------------------------------------------------------------------
[<AutoOpen>]
module TheGamma.GoogleCharts.Options

open System

type ChartSizeAxis = {
    maxSize : float
    maxValue : float
    minSize : float
    minValue : float
}

type ChartTextStyle = {
    fontName : string
    fontSize : float
    bold : bool
    italic : bool
    color : string
    auraColor : string
    opacity : float
}

type ChartTooltip = {
    isHtml : bool
    showColorCode : bool
    textStyle : ChartTextStyle
    trigger : string
}

type ChartLegend = {
    alignment : string
    maxLines : float
    position : string
    textStyle : ChartTextStyle
    numberFormat : string
}

type ChartColorAxis = {
    minValue : float
    maxValue : float
    values : float[]
    colors : string[]
    legend : ChartLegend
}

type GeoChartMagnifyingGlass = {
    enable : bool
    zoomFactor : float
}

type GeoChartOptions = {
    backgroundColor : obj
    colorAxis : ChartColorAxis
    datalessRegionColor : string
    displayMode : string
    enableRegionInteractivity : bool
    height : float
    keepAspectRatio : bool
    legend : ChartLegend
    region : string
    magnifyingGlass : GeoChartMagnifyingGlass
    markerOpacity : float
    resolution : string
    sizeAxis : ChartSizeAxis
    tooltip : ChartTooltip
    width : float
}

type ChartBoxStyleGradient = {
    color1: string
    color2: string
    x1: string
    y1: string
    x2: string
    y2: string
    useObjectBoundingBoxUnits : bool
}
type ChartBoxStyle = {
    stroke : string
    strokeWidth : float
    rx : float
    ry : float
    gradient : ChartBoxStyleGradient
}

type ChartAnnotations = {
    boxStyle : ChartBoxStyle
    textStyle : ChartTextStyle
}

type ChartCrosshairFocused = {
    color : string
    opacity : float
}
type ChartCrosshairSelected = {
    color : string
    opacity : float
}
type ChartCrosshair = {
    color : string
    focused : ChartCrosshairFocused
    opacity : float
    orientation : string
    selected : ChartCrosshairSelected 
    trigger : string
}

type ChartExplorer = {
    actions : string[]
    axis : string
    keepInBounds : bool
    maxZoomIn : float
    maxZoomOut : float
    zoomDelta : float
}

type ChartStroke = {
    stroke: string
    strokeWidth: float
    fill: string
}

type ChartArea = {
    top : obj
    left : obj
    width : obj
    height : obj
}


type TransitionAnimation = {
    duration : float
    easing : string // linear, in, out, inAndOut
}

type ChartGridlines = {
    color : string
    count : float
}

type ChartViewWindow = {
    max : float
    min : float
}

type ChartAxis = {
    baseline : float // This option is only supported for a continuous axis. https://google-developers.appspot.com/chart/interactive/docs/customizing_axes#Terminology
    baselineColor : string // google's documentation on this is wrong, specifies it as a number. The color of the baseline for the horizontal axis. Can be obj HTML color string, for example: 'red' or '#00cc00'
    direction : float // The direction in which the values along the horizontal axis grow. Specify -1 to reverse the order of the values.
    format : string // icu pattern set http://icu-project.org/apiref/icu4c/classDecimalFormat.html#_details
    gridlines : ChartGridlines
    minorGridlines : ChartGridlines
    logScale : bool
    textPosition : string
    textStyle : ChartTextStyle
    ticks : obj[]
    title : string
    titleTextStyle : ChartTextStyle
    allowContainerBoundaryTextCufoff : bool
    slantedText : bool
    slantedTextAngle : float
    maxAlternation : float
    maxTextLines : float
    minTextSpacing : float
    showTextEvery : float
    maxValue : float
    minValue : float
    viewWindowMode : string
    viewWindow : ChartViewWindow
}

type ChartBoundingBox = {
    left: float
    top: float
    width: float
    height: float
}

type GroupWidth = {
    groupWidth: obj // number | string
}

type Trendline = {
    color: string
    lineWidth: float
    labelInLegend : string
    opacity: float
    pointSize: float 
    pointsVisible: bool
    showR2 : bool
    ``type``: string
    visibleInLegend: float
}

type ScatterChartOptions = {
    aggregationTarget : string
    animation : TransitionAnimation
    annotations : ChartAnnotations
    axisTitlesPosition : string // in, out, none
    backgroundColor : obj
    chartArea : ChartArea
    colors : string[]
    crosshair : ChartCrosshair
    curveType : string
    dataOpacity : float
    enableInteractivity : bool
    explorer : ChartExplorer
    fontSize : float
    fontName : string
    forceIFrame : bool
    hAxis : ChartAxis
    height : float
    legend : ChartLegend
    lineWidth : float
    pointSize : float
    selectionMode : string
    series : obj
    theme : string
    title : string
    titlePosition : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    trendlines : Trendline[]
    vAxis : ChartAxis
    width : float
}

type ColumnChartOptions = {
    aggregationTarget : string
    animation : TransitionAnimation
    annotations : ChartAnnotations
    axisTitlesPosition : string // in, out, none
    backgroundColor : obj
    bar : GroupWidth
    chartArea : ChartArea
    colors : string[]
    enableInteractivity : bool
    focusTarget : string
    fontSize : float
    fontName : string
    hAxis : ChartAxis
    height : float
    isStacked : bool
    legend : ChartLegend
    reverseCategories : bool
    selectionMode : string // single / multiple
    series : obj
    theme : string
    title : string
    titlePosition : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    vAxes : obj
    vAxis : ChartAxis
    width : float
}

type LineChartOptions = {
    aggregationTarget : string
    animation : TransitionAnimation
    annotations : ChartAnnotations
    axisTitlesPosition : string
    backgroundColor : obj
    chartArea : ChartArea
    colors : string[]
    crosshair : ChartCrosshair
    curveType : string
    dataOpacity : float
    enableInteractivity : bool
    explorer : ChartExplorer
    focusTarget : string
    fontSize : float
    fontName : string
    hAxis : ChartAxis
    height : float
    interpolateNulls : bool
    legend : ChartLegend
    lineWidth : float
    orientation : string
    pointSize : float
    reverseCategories : bool
    selectionMode : string // single / multiple
    series : obj
    theme : string
    title : string
    titlePosition : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    vAxes : obj
    vAxis : ChartAxis
    width : float
}

type BarChartOptions = {
    aggregationTarget : string
    animation : TransitionAnimation
    annotations : ChartAnnotations
    axisTitlesPosition : string // in, out, none
    backgroundColor : obj
    bar : GroupWidth
    chartArea : ChartArea
    colors : string[]
    dataOpacity : float
    enableInteractivity : bool
    focusTarget : string
    fontSize : float
    fontName : string
    hAxes : obj
    hAxis : ChartAxis
    height : float
    isStacked : bool
    legend : ChartLegend
    reverseCategories : bool
    series : obj
    theme : string
    title : string
    titlePosition : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    vAxes : obj
    vAxis : ChartAxis
    width : float
}

type HistogramHistogram = {
    bucketSize : float
    hideBucketItems : bool
    lastBucketPercentile : float
}

type HistogramOptions = {
    animation : TransitionAnimation
    axisTitlesPosition : string // in, out, none
    backgroundColor : obj
    bar : GroupWidth
    chartArea : ChartArea
    colors : string[]
    dataOpacity : float
    enableInteractivity : bool
    focusTarget : string
    fontSize : float
    fontName : string
    hAxis : ChartAxis
    histogram : HistogramHistogram
    height : float
    interpolateNulls : bool
    isStacked : bool
    legend : ChartLegend
    orientation : string
    reverseCategories : bool
    series : obj
    theme : string
    title : string
    titlePosition : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    vAxes : obj
    vAxis : ChartAxis
    width : float
}

type AreaChartOptions = {
    aggregationTarget : string
    animation : TransitionAnimation
    areaOpacity : float
    axisTitlesPosition : string
    backgroundColor : obj
    chartArea : ChartArea
    colors : string[]
    crosshair : ChartCrosshair
    dataOpacity : float
    enableInteractivity : bool
    explorer : ChartExplorer
    focusTarget : string
    fontSize : float
    fontName : string
    hAxis : ChartAxis
    height : float
    interpolateNulls : bool
    isStacked : bool
    legend : ChartLegend
    lineWidth : float
    orientation : string
    pointSize : float
    reverseCategories : bool
    selectionMode : string // single / multiple
    series : obj
    theme : string
    title : string
    titlePosition : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    vAxes : obj
    vAxis : ChartAxis
    width : float
}

type AnnotationChartOptions = {
    allowHtml : bool
    allValuesSuffix : string
    annotationsWidth : float
    colors : string[]
    dateFormat : string
    displayAnnotations : bool
    displayAnnotationsFilter : bool
    displayDateTimeBarSeparator : bool
    displayExactValues : bool
    displayLegendDots : bool
    displayLegendValues : bool
    displayRangeSelector : bool
    displayZoomButtons : bool
    fill : float
    legendPosition : string
    max : float
    min : float
    numberFormats : obj
    scaleColumns : float[]
    scaleFormat : string
    scaleType : string
    thickness : float
    zoomEndTime : DateTime
    zoomStartTime : DateTime
}

type SteppedAreaChartOptions = {
    aggregationTarget : string
    animation : TransitionAnimation
    areaOpacity : float
    axisTitlesPosition : string
    backgroundColor : obj
    chartArea : ChartArea
    colors : string[]
    connectSteps : bool
    enableInteractivity : bool
    focusTarget : string
    fontSize : float
    fontName : string
    hAxis : ChartAxis
    height : float
    interpolateNulls : bool
    isStacked : bool
    legend : ChartLegend
    reverseCategories : bool
    selectionMode : string // single / multiple
    series : obj
    theme : string
    title : string
    titlePosition : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    vAxes : obj
    vAxis : ChartAxis
    width : float
}

type PieChartOptions = {
    backgroundColor : obj
    chartArea : ChartArea
    colors : string[]
    enableInteractivity : bool
    fontSize : float
    fontName : string
    height : float
    is3D : bool
    legend : ChartLegend
    pieHole : float
    pieSliceBorderColor : string
    pieSliceText : string
    pieSliceTextStyle : ChartTextStyle
    pieStartAngle : float
    reverseCategories : bool
    pieResidueSliceColor : string
    pieResidueSliceLabel : string
    slices : obj
    sliceVisibilityThreshold : float
    title : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    width : float
}

type ChartBubble = {
    opacity : float
    stroke : string
    textStyle : ChartTextStyle
}

type BubbleChartOptions = {
    animation : TransitionAnimation
    axisTitlesPosition : string // in, out, none
    backgroundColor : obj
    bubble : ChartBubble
    chartArea : ChartArea
    colors : string[]
    colorAxis : ChartColorAxis
    enableInteractivity : bool
    explorer : ChartExplorer
    fontSize : float
    fontName : string
    forceIFrame : bool
    hAxis : ChartAxis
    height : float
    legend : ChartLegend
    selectionMode : string
    series : obj
    sizeAxis : ChartSizeAxis
    sortBubblesBySize : bool
    theme : string
    title : string
    titlePosition : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    vAxis : ChartAxis
    width : float
}

type TreeMapOptions = {
    fontColor : string
    fontFamily : string
    fontSize : float
    forceIFrame : bool
    headerColor : string
    headerHeight : float
    headerHighlightColor : string
    hintOpacity : float
    maxColor : string
    maxDepth : float
    maxHighlightColor : string
    maxPostDepth : float
    maxColorValue : float
    midColor : string
    midHighlightColor : string
    minColor : string
    minHighlightColor : string
    minColorValue : float
    showScale : bool
    showTooltips : bool
    textStyle : ChartTextStyle
    title : string
    titleTextStyle : ChartTextStyle
    useWeightedAverageForAggregation : bool
}

type CssClassNames = {
    headerRow : string
    tableRow : string
    oddTableRow : string
    selectedTableRow : string
    hoverTableRow : string
    headerCell : string
    tableCell : string
    rowNumberCell : string
}

type TableOptions = {
    allowHtml : bool
    alternatingRowStyle : bool
    cssClassName : CssClassNames
    firstRowNumber : float
    height : string
    page : string
    pageSize : float
    rtlTable : bool
    scrollLeftStartPosition : float
    showRowNumber : bool
    sort : string
    sortAscending : bool
    sortColumn : float
    startPage : float
    width : string
}

type LabelStyle = {
    color: string
    fontName: string
    fontSize: string
}

type TimelineTimeline = {
    barLabelStyle : LabelStyle
    colorByRowLabel : bool
    groupByRowLabel : bool
    rowLabelStyle : LabelStyle
    showRowLabels : bool
    singleColor : string
}

type TimelineOptions = {
    avoidOverlappingGridLines : bool
    backgroundColor : obj
    colors : string[]
    enableInteractivity : bool
    forceIFrame : bool
    height : float
    timeline : TimelineTimeline
    width : float
}

type CandlestickCandlestick = {
    hollowIsRising : bool
    fallingColor : ChartStroke
    risingColor : ChartStroke
}

type CandlestickChartOptions = {
    aggregationTarget : string
    animation : TransitionAnimation
    axisTitlesPosition : string
    backgroundColor : obj
    bar : GroupWidth
    candlestick : CandlestickCandlestick
    chartArea : ChartArea
    colors : string[]
    enableInteractivity : bool
    focusTarget : string
    fontSize : float
    fontName : string
    hAxis : ChartAxis
    height : float
    legend : ChartLegend
    orientation : string
    reverseCategories : bool
    selectionMode : string // single / multiple
    series : obj
    theme : string
    title : string
    titlePosition : string
    titleTextStyle : ChartTextStyle
    tooltip : ChartTooltip
    vAxes : obj
    vAxis : ChartAxis
    width : float
}
    