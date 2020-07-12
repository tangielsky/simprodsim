unit SimController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls,
  SimTime,
  SimObjects,
  SimSource,
  SimSink,
  SimBuffer,
  SimOrder;

type
  TSimulationController = class
  private
    FFinished : boolean;
    FTimeInterval : longint;
    FSource : TSimulationSource;
    FSink : TSimulationSink;
    FTime : TSimulationTime;
    FViewWip : TWinCOntrol;
    procedure SetViewWip(AValue: TWinControl);
  public
    constructor Create;
    destructor Destroy;
    procedure DoNextStep;
    procedure Reset;
    procedure UpdateViewWip;
  published
    property Finished : boolean read FFinished write FFinished;
    property Sink : TSimulationSink read FSink write FSink;
    property Source : TSimulationSource read FSource write FSource;
    property Time : TSimulationTime read FTime write FTime;
    property TimeInterval : longint read FTimeInterval write FTimeInterval;
    property ViewWip : TWinControl read FViewWip write SetViewWip;
end;


var
  SimulationController : TSimulationController;

implementation

uses
  SimArticle,
  SimWorkcenter;

procedure TSimulationController.SetViewWip(AValue: TWinControl);
begin
  if FViewWip=AValue then Exit;
  FViewWip:=AValue;
  UpdateViewWip;
end;

constructor TSimulationController.Create;
begin
  inherited Create;
  FTime:=TSimulationTime.Create;
  FSink:=nil;
  FSource:=nil;
  FViewWip:=nil;
  FTimeInterval:=1;

  Reset;
end;

destructor TSimulationController.Destroy;
begin
  FTime.Free;
  inherited Destroy;
end;

procedure TSimulationController.Reset;
begin
  FFinished:=false;
  FTime.Reset;
  OrderList.Reset;
end;


procedure TSimulationController.DoNextStep;
var
  i : integer;
  Workcenter,NextWorkcenter : TWorkcenter;
  Order : TOrder;
  OrderItem : TOrderItem;
  Buffer : TSimulationBuffer;
begin
  FTime.Second:=FTime.Second+FTimeInterval;

  for i:=0 to WorkcenterList.Count-1 do
    begin
      Workcenter:=TWorkcenter(WorkcenterList[i]);
      Workcenter.DoNextStep;
      Workcenter.UpdateLabels;

      //Event handling:

      //Is there something in outputbuffer? Should it be transported?
      if Workcenter.OutputBuffer.Orders.Count>0 then
        begin
          if Workcenter.OutputBuffer.IsOrderAllowed(0) then //FIFO
            begin
              Order:=Workcenter.OutputBuffer.GetOrder(0);
              OrderItem:=Order.GetNextOrderItem;

              if OrderItem=nil then NextWorkcenter:=nil
              else NextWorkcenter:=WorkcenterList.FindById(OrderItem.WorkcenterId);

              if NextWorkcenter=nil then //order is ready, move to sink
                begin
                  FSink.AddOrder(Workcenter.OutputBuffer.ForwardOrder(0));
                  UpdateViewWip;
                 end
              else
                begin
                  Buffer:=NextWorkcenter.InputBuffer; //next operation
                  if Buffer.IsFree then
                    Buffer.AddOrder(Workcenter.OutputBuffer.ForwardOrder(0))
                  else
                    begin
                      //if inputbuffer=0, then direct to workcenter?
                      if (NextWorkcenter.InputBuffer.MaxOrders=0) and (NextWorkcenter.ActiveOrder=nil) then
                        NextWorkcenter.ActiveOrder:=Workcenter.OutputBuffer.ForwardOrder(0);
                    end;
                end;
            end;
        end;

        //ist outputbuffer =0, then check if input is free
        if (Workcenter.OutputBuffer.MaxOrders=0) and (Workcenter.ActiveOrder<>nil) and (Workcenter.State=wcsWait) then
          begin
            OrderItem:=Workcenter.ActiveOrder.GetNextOrderItem;
            if OrderItem=nil then NextWorkcenter:=nil
            else NextWorkcenter:=WorkcenterList.FindById(OrderItem.WorkcenterId);

            if NextWorkcenter=nil then //order is ready, move to sink
              begin
                FSink.AddOrder(Workcenter.ForwardOrder);
                UpdateViewWip;
               end
            else
              begin
                Buffer:=NextWorkcenter.InputBuffer; //next operation
                if Buffer.IsFree then
                  Buffer.AddOrder(Workcenter.ForwardOrder)
                else
                  begin
                    //if inputbuffer=0, then direct to workcenter?
                    if (NextWorkcenter.InputBuffer.MaxOrders=0) and (NextWorkcenter.ActiveOrder=nil) then
                      NextWorkcenter.ActiveOrder:=Workcenter.ForwardOrder;
                  end;
              end;
           end;
        if (Workcenter.InputBuffer.IsFree)
              or ((Workcenter.InputBuffer.MaxOrders=0) and (Workcenter.ActiveOrder=nil)) then
                //get an order from source?
              begin
                 if Workcenter.InputBuffer.IsFree then
                   begin
                     Order:=SimulationController.Source.GetNextOrderForWorkcenterId(Workcenter.Id);
                     if Order<>nil then
                       begin
                         Workcenter.InputBuffer.AddOrder(Order);
                         UpdateViewWip;
                       end;
                   end
                 else if ((Workcenter.InputBuffer.MaxOrders=0) and (Workcenter.ActiveOrder=nil)) then
                   begin
                     Workcenter.ActiveOrder:=SimulationController.Source.GetNextOrderForWorkcenterId(Workcenter.Id);
                     if Workcenter.ActiveOrder<>nil then UpdateViewWip;
                   end;
              end;

        //Simulation finished?
        if OrderList.IsFinished then FFinished:=true;

    end;
end;


procedure TSimulationController.UpdateViewWip;
var
  i : integer;
  Order : TOrder;
  buf : TSimulationBuffer;
begin
  if FViewWip=nil then exit;
  if FViewWip is TListview then
    begin
      buf:=TSimulationBuffer.Create;

      for i:=0 to OrderList.Count-1 do
        begin
          Order:=TOrder(OrderList[i]);
          if (FSource.Buffer.FindByOrderId(Order.Id)=false) and (FSink.Buffer.FindByOrderId(Order.Id)=false) then
            buf.Orders.Add(Order);
        end;
      buf.View:=FViewWip as TListview;
      buf.UpdateView;
      buf.Free;
    end;
end;





initialization
  SimulationController:=TSimulationController.Create;


finalization
  SimulationController.Free;


end.

