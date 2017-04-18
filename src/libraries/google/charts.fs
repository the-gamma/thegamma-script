namespace TheGamma.GoogleCharts

open System
open TheGamma
open TheGamma.Series
open TheGamma.GoogleCharts

type chart =
  static member scatter(xval:series<_, _>, yval:series<_, _>) = 
    { Scatter.data = ChartDataOperations.twoValues xval yval; 
      typeName = "ScatterChart"; options = ScatterChartOptions.empty }

  static member geo(data:series<string, float>) = 
    { Geo.data = ChartDataOperations.oneKeyValue "string" data; 
      typeName = "GeoChart"; options = GeoChartOptions.empty }
  (*
  static member geo(data:series<string, float * float>) = 
    { Geo.data = ChartDataOperations.oneKeyTwoValues "string" data; 
      typeName = "GeoChart"; options = GeoChartOptions.empty }
  *)
  static member pie(data:series<string, float>) = 
    { Pie.data = ChartDataOperations.oneKeyValue "string" data; 
      typeName = "PieChart"; options = PieChartOptions.empty }

  static member bar(data:series<string, float>) = 
    { Bar.data = ChartDataOperations.oneKeyValue "string" data; 
      typeName = "BarChart"; options = BarChartOptions.empty }
  (*
  static member bar(data:seq<series<string, float>>) = 
    { Bar.data = ChartDataOperations.oneKeyNValues "string" data; 
      typeName = "BarChart"; options = BarChartOptions.empty }
  static member column(data:seq<series<string, float>>) = 
    { Column.data = ChartDataOperations.oneKeyNValues "string" data; 
      typeName = "ColumnChart"; options = ColumnChartOptions.empty }
  *)
  static member column(data:series<string, float>) = 
    { Column.data = ChartDataOperations.oneKeyValue "string" data; 
      typeName = "ColumnChart"; options = ColumnChartOptions.empty }

  static member columns(data:series<string, float>[], colors:string[]) = 
    { Line.data = ChartDataOperations.oneKeyAppendValues "string" data colors; 
      typeName = "ColumnChart"; options = LineChartOptions.empty }

  static member line(data:series<int, float>) = 
    { Line.data = ChartDataOperations.oneKeyValue "number" data; 
      typeName = "LineChart"; options = LineChartOptions.empty }

  static member lines(data:series<'a, series<'b, float>>) = 
    { Line.data = ChartDataOperations.oneKeyNValues "number" data; 
      typeName = "LineChart"; options = LineChartOptions.empty }
(*
  static member line(data:series<string, float>) = 
    { Line.data = ChartDataOperations.oneKeyValue "string" data; 
      typeName = "LineChart"; options = LineChartOptions.empty }
  static member line(data:seq<series<string, float>>) = 
    { Line.data = ChartDataOperations.oneKeyNValues "string" data; 
      typeName = "LineChart"; options = LineChartOptions.empty }
  static member line(data:seq<series<int, float>>) = 
    { Line.data = ChartDataOperations.oneKeyNValues "number" data; 
      typeName = "LineChart"; options = LineChartOptions.empty }
*)
(*
  static member histogram(data) = 
    { Histogram.data = data; options = HistogramOptions.empty }
*)
  static member area(data:series<int, float>) = 
    { Area.data = ChartDataOperations.oneKeyValue "number" data; 
      typeName = "AreaChart"; options = AreaChartOptions.empty }

  static member areas(data:series<'a, series<int, float>>, ?names:string[]) = 
    let i = ref 0;
    let data = 
      match names with 
      | Some names -> data.map(fun s -> incr i; s.setProperties(seriesName=names.[i.Value-1]))
      | None -> data
    { Area.data = ChartDataOperations.oneKeyNValues "number" data; 
      typeName = "AreaChart"; options = AreaChartOptions.empty }
(*
  static member annotation(data) = 
    { Annotation.data = data; options = AnnotationChartOptions.empty }
  static member steppedArea(data) = 
    { SteppedArea.data = data; options = SteppedAreaChartOptions.empty }
  static member bubble(data) = 
    { Bubble.data = data; options = BubbleChartOptions.empty }
  static member treeMap(data) = 
    { TreeMap.data = data; options = TreeMapOptions.empty }
  static member table(data) = 
    { Table.data = data; options = TableOptions.empty }
  static member timeline(data) = 
    { Timeline.data = data; options = TimelineOptions.empty }
  static member candlestick(data) = 
    { Candlestick.data = data; options = CandlestickChartOptions.empty }
*)

  static member show(chart:#Chart) = 
    Helpers.showChart(chart)