unit SimSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls,
  SimTime,
  SimObjects,
  SimOrder,
  SimBuffer;

type
  TSimulationSource = class
  private
    FBuffer : TSimulationBuffer;
    FView : TWinControl;
    procedure SetView(AValue: TWinControl);
    procedure UpdateView;
  public
    constructor Create;
    destructor Destroy;
    function GetNextOrderForWorkcenterId(WorkcenterId : string) : TOrder;
    procedure Reset;
  published
    property Buffer : TSimulationBuffer read FBuffer write FBuffer;
    property View : TWinControl read FView write SetView;
end;


implementation

uses SimController;


constructor TSimulationSource.Create;
begin
  inherited Create;

  FBuffer:=TSimulationBuffer.Create;
  FBuffer.MaxOrders:=-1;
  FBuffer.MinStayInTime:=0;

  FView:=nil;
end;

destructor TSimulationSource.Destroy;
begin
  FBuffer.Free;

  inherited Destroy;
end;


procedure TSimulationSource.SetView(AValue: TWinControl);
begin
  if FView=AValue then Exit;
  FView:=AValue;
  FBuffer.View:=FView;
end;

procedure TSimulationSource.Reset;
begin
  FBuffer.Orders.Clear;
  if FView=nil then exit;
  if FView is TListview then (FView as TListView).Items.Clear;
end;

function TSimulationSource.GetNextOrderForWorkcenterId(WorkcenterId: string
  ): TOrder;
var
  i : integer;
  Order : TOrder;
  OrderItem : TOrderItem;
begin
  result:=nil;
  for i:=0 to FBuffer.Orders.Count-1 do
    begin
      Order:=TOrder(FBuffer.Orders[i]);
      OrderItem:=Order.GetNextOrderItem;
      if (OrderItem<>nil) then
        if OrderItem.WorkcenterId=WorkcenterId then
          begin
            Order.Starttime.Assign(SimulationController.Time);
            result:=FBuffer.ForwardOrder(i);
            exit;
          end;
    end;
end;


procedure TSimulationSource.UpdateView;
var
  i,j : integer;
  Listview : TListview;
  ListItem : TListItem;
  Order : TOrder;
  OrderState : TOrderState;
begin
  if (FView=nil) then exit;

  if FView is TListview then
    begin
      Listview:=FView as TListview;
      Listview.BeginUpdate;
      for i:=0 to FBuffer.Orders.Count-1 do
        begin
          Order:=TOrder(FBuffer.Orders[i]);
          for j:=0 to Listview.Items.Count-1 do
            if Listview.Items[j].Caption=Order.Id then
              begin
                OrderState:=Order.GetOrderState;
                if OrderState=osInProcess then Listview.Items[j].ImageIndex:=1
                else if OrderState=osFinished then Listview.Items[j].ImageIndex:=2
                else Listview.Items[j].ImageIndex:=0;

                Listview.Items[j].SubItems[0]:=Order.Starttime.FormatStr;
                Listview.Items[j].SubItems[1]:=Order.Endtime.FormatStr;
                break;
              end;

        end;
      Listview.EndUpdate;
    end;
end;



end.

