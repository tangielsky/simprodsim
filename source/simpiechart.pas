unit SimPieChart;

{$mode objfpc}{$H+}

interface

uses
  Windows, LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls, Messages, Graphics,
  ExtCtrls, Dialogs;

type
  TSimPieChartValue = record
    Caption : string;
    Value : longint;
    Color : TColor;
    ValueUnit : string;
    ValuePercentage : integer;
  end;

  TSimPieChartShowTextStyle = (spcValue,spcPercentage);

  TSimPieChart = class(TCustomControl)
  private
    FValueCount : integer;
    FStartAngle: Integer;
    FInnerSize: Integer;
    FShowText: Boolean;
    FInfo : string;
    FShowTextStyle : TSimPieChartShowTextStyle;
    function GetValueCount: integer;
    procedure SetStartAngle(const Value: Integer);
    procedure SetInnerSize(const Value: Integer);
    procedure SetShowText(const Value: Boolean);
    procedure SetValueCount(AValue: integer);
  protected
  public
    Values : Array of TSimPieChartValue;
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure AddValue(no :integer; ACaption: string; AValueUnit : string; AColor: TColor);
    procedure EditValue(no : integer; AValue : longint);
  published
    property StartAngle: Integer read FStartAngle write SetStartAngle;
    property InnerSize: Integer read FInnerSize write SetInnerSize;
    property ShowText: Boolean read FShowText write SetShowText;
    property ShowTextStyle : TSimPieChartShowTextStyle read FShowTextStyle write FShowTextStyle;
    property ValueCount : integer read GetValueCount write SetValueCount;
    property Info : string read FInfo;
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



constructor TSimPieChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered:=True;
  SetLength(Values,1);
  Color:=clWhite;
  Font.Color:=clBlack;
  Font.Size:=7;
  FShowText:=true;
  FShowTextStyle:=spcValue;
  FInnersize:=60;
  FStartAngle:=90;
end;


procedure TSimPieChart.SetStartAngle(const Value: Integer);
var
  V: Integer;
begin
  V:=Value;
  if V<0 then V:=0;
  if V>359 then V:=359;
  if FStartAngle<>V then
  begin
    FStartAngle:=V;
    Invalidate;
  end;
end;

function TSimPieChart.GetValueCount: integer;
begin
  result:=Length(Values);
end;

procedure TSimPieChart.SetInnerSize(const Value: Integer);
var
  V: Integer;
begin
  V:=Value;
  if V<0 then V:=0;
  if V>99 then V:=99;
  if FInnerSize<>V then
  begin
    FInnerSize:=V;
    Invalidate;
  end;
end;

procedure TSimPieChart.SetShowText(const Value: Boolean);
begin
  if FShowText<>Value then
  begin
    FShowText:=Value;
    Invalidate;
  end;
end;

procedure TSimPieChart.SetValueCount(AValue: integer);
begin
  if FValueCount=AValue then Exit;
  SetLength(Values,AValue);
  Invalidate;
end;

procedure TSimPieChart.Paint;
var
  Buf: TBitmap;
  R,RR: TRect;
  P1,P2: TPoint;
  S: string;
  i : integer;
  sum,vsum,sangle,r1,r2 : double;
  ra : integer;

begin
  inherited Paint;

  if Width>Height then R:=Rect(0,0,Height,Height)
  else R:=Rect(0,0,Width,Width);

  sangle:=(FStartAngle/360)*(2*PI);

  Buf:=TBitmap.Create;
  Buf.Width:=R.Right;
  Buf.Height:=R.Bottom;
  with Buf.Canvas do
    begin
      Brush.Color:=self.Color;
      Rectangle(0,0,Buf.Width,Buf.Height);
      Pen.Style:=psClear;

      sum:=0;
      for i:=low(Values) to high(Values) do
        sum:=sum+Values[i].Value;

      if sum>0 then
        begin
          vsum:=0;
          for i:=low(Values) to high(Values) do
            begin
              Values[i].ValuePercentage:=Round(Values[i].Value/sum*100);
              vsum:=vsum+Values[i].ValuePercentage;
              if Values[i].ValuePercentage>0 then
                begin
                  Pen.Color:=Values[i].Color;
                  Brush.Color:=Values[i].Color;
                  r1:=vsum/100*2*PI-sangle;
                  r2:=(vsum-Values[i].ValuePercentage)/100*2*PI-sangle;
                  ra:=Width div 2;
                  P1.X:=ra+Trunc((ra*cos(r1)));
                  P1.Y:=ra+Trunc((ra*sin(r1)));
                  P2.X:=ra+Trunc((ra*cos(r2)));
                  P2.Y:=ra+Trunc((ra*sin(r2)));
                  finfo:=IntToStr(P1.X)+'|'+IntToStr(P1.Y)+'  '+IntToStr(P2.X)+'|'+IntToStr(P2.Y);
                  Pie(R.Left,R.Top,R.Right,R.Bottom,P1.X,P1.Y,P2.X,P2.Y);
                end;
            end;
          if InnerSize>0 then
          begin
            RR:=Rect(
              R.Left+(100-InnerSize)*(R.Right-R.Left) div 200,
              R.Top+(100-InnerSize)*(R.Bottom-R.Top) div 200,
              R.Right-(100-InnerSize)*(R.Right-R.Left) div 200,
              R.Bottom-(100-InnerSize)*(R.Bottom-R.Top) div 200);
            Brush.Color:=self.Color;
            Ellipse(RR.Left,RR.Top,RR.Right,RR.Bottom);
          end;
        end;

      if FShowText and (Length(Values)>0) then
        begin
          Font.Assign(self.Font);
          if ShowTextStyle=spcValue then
            S:=IntToStr(Values[0].Value)+Values[0].ValueUnit
          else
            begin
              if sum=0 then r1:=0
              else r1:=(Values[0].Value/sum)*100;
              S:=FormatFloat('0.0%',r1);
            end;
          TextOut((Width-TextWidth(S)) div 2,(Height-TextHeight(S)) div 2,s);
        end;

    end;

  Canvas.CopyRect(Rect(0,0,Width,Height),Buf.Canvas,Rect(0,0,Buf.Width,Buf.Height));

  Buf.Free;
end;

procedure TSimPieChart.AddValue(no: integer; ACaption: string; AValueUnit : string; AColor: TColor);
begin
  SetLength(Values,no+1);
  Values[no].Caption:=ACaption;
  Values[no].ValueUnit:=AValueUnit;
  Values[no].Color:=AColor;
end;

procedure TSimPieChart.EditValue(no: integer; AValue : longint);
begin
  Values[no].Value:=AValue;
end;

procedure Register;
begin
  RegisterComponents('Mylib', [TSimPieChart]);
end;

end.
