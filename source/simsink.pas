unit SimSink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls,
  SimTime,
  SimObjects,
  SimOrder,
  SimBuffer;

type
  TSimulationSink = class
  private
    FBuffer : TSimulationBuffer;
    FView : TWinControl;
    procedure SetView(AValue: TWinControl);
  public
    constructor Create;
    destructor Destroy;
    procedure AddOrder(Order : TOrder);
    procedure Reset;
  published
    property Buffer : TSimulationBuffer read FBuffer write FBuffer;
    property View : TWinControl read FView write SetView;

end;


implementation

uses SimController;


procedure TSimulationSink.SetView(AValue: TWinControl);
begin
  if FView=AValue then Exit;
  FView:=AValue;
  FBuffer.View:=FView;
end;

constructor TSimulationSink.Create;
begin
  inherited Create;

  FBuffer:=TSimulationBuffer.Create;
  FBuffer.MaxOrders:=-1;
  FBuffer.MinStayInTime:=0;

  FView:=nil;
end;

destructor TSimulationSink.Destroy;
begin
  FBuffer.Free;

  inherited Destroy;
end;


procedure TSimulationSink.Reset;
begin
  FBuffer.Orders.Clear;
  if (FView=nil) then exit;
  if FView is TListview then (FView as TListView).Items.Clear;
end;



procedure TSimulationSink.AddOrder(Order: TOrder);
begin
  if Order=nil then exit;

  FBuffer.AddOrder(Order);
  Order.Endtime.Assign(SimulationController.Time);

  SimulationController.UpdateViewWip;
end;




end.

