unit SimXYChart;

{$mode objfpc}{$H+}

interface

uses
  Windows, LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Messages, Graphics,
  ExtCtrls, Dialogs,
  SimObjects, SimController;

type

  { TSimXYChart }
  TSimXYChartType = (sctLines,sctAreas);
  TSimXYChartView = (scvAll,scvRange,scvRunning);

  TSimXYChart = class(TCustomControl)
  private
    FChartColor : TColor;
    FDataColor : TColor;
    FSimData : TSimulationData;
    FChartType : TSimXYChartType;
    FView : TSimXYChartView;
    FRangeFrom : longint;
    FRangeTo : longint;
    FRangeWidth : longint;
    FUseMaxTimeAsCurrentSimulationTime : boolean;
    procedure SetChartColor(AValue: TColor);
    procedure SetChartType(AValue: TSimXYChartType);
    procedure SetDataColor(AValue: TColor);
    procedure SetValues(AValue: TSimulationData);
    procedure SetView(AValue: TSimXYChartView);
  public
    function GetMaxTime: longint;
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property ChartColor : TColor read FChartColor write SetChartColor;
    property ChartType : TSimXYChartType read FChartType write SetChartType;
    property DataColor : TColor read FDataColor write SetDataColor;
    property View : TSimXYChartView read FView write SetView;
    property RangeFrom : longint read FRangeFrom write FRangeFrom;
    property RangeTo : longint read FRangeTo write FRangeTo;
    property RangeWidth : longint read FRangeWidth write FRangeWidth;
    property UseMaxTimeAsCurrentSimulationTime : boolean read FUseMaxTimeAsCurrentSimulationTime write FUseMaxTimeAsCurrentSimulationTime;
    property Values: TSimulationData read FSimData write SetValues;
    property Align;
    property Anchors;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentFont;
    property Color;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property Visible;
    property UseDockManager;
  end;

procedure Register;

implementation

{ TSimXYChart }

procedure TSimXYChart.SetValues(AValue: TSimulationData);
begin
  if FSimData=AValue then Exit;
  FSimData:=AValue;

  Invalidate;
end;

procedure TSimXYChart.SetView(AValue: TSimXYChartView);
begin
  if FView=AValue then Exit;
  FView:=AValue;
  Invalidate;
end;

procedure TSimXYChart.SetDataColor(AValue: TColor);
begin
  if FDataColor=AValue then Exit;
  FDataColor:=AValue;
  Invalidate;
end;

procedure TSimXYChart.SetChartColor(AValue: TColor);
begin
  if FChartColor=AValue then Exit;
  FChartColor:=AValue;
  Invalidate;
end;

procedure TSimXYChart.SetChartType(AValue: TSimXYChartType);
begin
  if FChartType=AValue then Exit;
  FChartType:=AValue;
  Invalidate;
end;

constructor TSimXYChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered:=True;
  Color:=clWhite;
  Font.Color:=clBlack;
  Font.Size:=7;
  FChartColor:=clGray;
  FDataColor:=clGreen;
  FChartType:=sctAreas;
  FUseMaxTimeAsCurrentSimulationTime:=true;

  FRangeWidth:=0;
  FRangeFrom:=0;
  FRangeTo:=0;
  FView:=scvAll;

end;

function TSimXYChart.GetMaxTime : longint;
begin
  result:=0;
  if FSimData=nil then exit;

  if FUseMaxTimeAsCurrentSimulationTime then
    result:=SimulationController.Time.Time
  else result:=FSimData.MaxTime;
end;

procedure TSimXYChart.Paint;
var
  Buf: TBitmap;
  i,xmax,ymax,xstart,ystart,a,ya,xa: integer;
  xvaluemax,yvaluemax: longint;
  xfrom,xto,x1,x2,y1,y2 : longint;
  s : string;

  function CalcX(value : longint) : longint;
  var k : longint;
  begin
    if (xto-xfrom)=0 then k:=0
    else k:=xstart+Round((value-xfrom)/(xto-xfrom)*xmax);
    if k<xstart then k:=xstart
    else if k>xstart+xmax then k:=xstart+xmax;
    result:=k;
  end;

  function CalcY(value : longint) : longint;
  begin
    if yvaluemax=0 then result:=0
    else result:=ystart-Round(value/yvaluemax*ymax);
  end;

begin
  inherited Paint;

  Buf:=TBitmap.Create;
  Buf.Width:=Width;
  Buf.Height:=Height;

  a:=Round(Buf.Width*0.005);
  if a=0 then a:=1;

  with Buf.Canvas do
    begin
      ya:=TextHeight('0');
      xa:=TextWidth('0000');

      ystart:=Buf.Height-ya;
      xstart:=xa;
      xmax:=Buf.Width-xstart-a*3;
      ymax:=ystart-a*3;


      Brush.Color:=self.Color;
      Pen.Color:=self.Color;
      Pen.Style:=psClear;
      Rectangle(0,0,Buf.Width+1,Buf.Height+1);

      //Draw axles
      Pen.Color:=FChartColor;
      Pen.Style:=psSolid;

      MoveTo(xstart,ystart);
      LineTo(xstart,ystart-ymax);
      LineTo(xstart-a,ystart-ymax+a*2);
      LineTo(xstart+a,ystart-ymax+a*2);
      LineTo(xstart,ystart-ymax);

      MoveTo(xstart,ystart);
      LineTo(xstart+xmax,ystart);
      LineTo(xstart+xmax-2*a,ystart-a);
      LineTo(xstart+xmax-2*a,ystart+a);
      LineTo(xstart+xmax,ystart);


      //Draw points
      if FSimData<>nil then
        begin
          if FView=scvRange then
            begin
              xfrom:=FRangeFrom;
              xto:=FRangeTo;
            end
          else if FView=scvRunning then
            begin
              xto:=GetMaxTime;
              xfrom:=GetMaxTime-FRangeWidth+1;
            end
          else
            begin //scpAll
              xfrom:=0;
              xto:=GetMaxTime;
            end;

          Pen.Color:=FDataColor;
          Brush.Color:=FDataColor;
          xvaluemax:=GetMaxTime;
          yvaluemax:=FSimData.MaxValue;

          for i:=1 to high(FSimData.Values) do
            begin
              if (FSimData.Values[i].Time>=xfrom) and (FSimData.Values[i].Time<=xto) then
                begin
                  x1:=CalcX(FSimData.Values[i-1].Time);
                  x2:=CalcX(FSimData.Values[i].Time);
                  y1:=CalcY(FSimData.Values[i-1].Value);
                  y2:=CalcY(FSimData.Values[i].Value);

                  if FChartType=sctLines then
                    begin
                      Line(x1,y1,x1,y2);
                      Line(x1,y2,x2,y2);
                    end else Rectangle(x1,CalcY(0),x2,y1);
                end;
            end;
          //Captions

          Font.Color:=FChartColor;
          Color:=self.Color;
          Pen.Color:=FChartColor;
          Brush.Color:=self.Color;
          TextOut(0,a,IntToStr(FSimData.MaxValue));
          TextOut(0,a+ya,FSimData.ValueUnit);
          TextOut(0,ystart-ya,'0');

          TextOut(xstart,ystart+1,IntToStr(xfrom)+' s');
          s:=IntToStr(xto)+' s';
          TextOut(xstart+xmax-TextWidth(s)-a,ystart+1,s);
        end;

    end;

  Canvas.CopyRect(Rect(0,0,Width,Height),Buf.Canvas,Rect(0,0,Buf.Width,Buf.Height));

  Buf.Free;
end;

procedure Register;
begin
  RegisterComponents('Mylib', [TSimXYChart]);
end;

end.
