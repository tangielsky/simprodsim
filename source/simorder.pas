unit SimOrder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SimArticle,
  SimTime,
  SimObjects;


type
  TOrderItem = class
    private
      FPos : integer;
      FWorkcenterId : string;
      FSetuptime : longint;
      FProcesstime : longint;
      FAmount : longint;
      FAmountFinished : longint;
      FTime : longint;
      FOrderState : TOrderState;
      FStarttime : TSimulationTime;
      FEndtime : TSimulationTime;
      procedure SetOrderState(AValue: TOrderState);
    public
      constructor Create;
      destructor Destroy;
      procedure Reset;
      procedure DoNextStep;
      function GetTotalTime : longint;
      procedure StartWorking;
    published
      property Pos : integer read FPos write FPos;
      property WorkcenterId : string read FWorkcenterId write FWorkcenterId;
      property Setuptime : longint read FSetuptime write FSetuptime;
      property Processtime : longint read FProcesstime write FProcesstime;
      property Amount : longint read FAmount write FAmount;
      property AmountFinished : longint read FAmountFinished;
      property Time : longint read FTime write FTime;
      property TotalTime : longint read GetTotalTime;
      property OrderState : TOrderState read FOrderState write SetOrderState;
      property Starttime : TSimulationTime read FStarttime;
      property Endtime : TSimulationTime read FEndtime;
      property State : TOrderState read FOrderState write FOrderState;
    end;



  TOrder = class(TList)
    private
      FId : string;
      FArticleId : string;
      FAmount : longint;
      FStarttime : TSimulationTime;
      FEndtime : TSimulationTime;
      FEnterBufferTime : TSimulationTime;
    public
      constructor Create;
      destructor Destroy;
      procedure Reset;
      function GetNextOrderItem : TOrderItem;
      function GetCurrentWorkcenterId : string;
      function GetOrderState : TOrderState;
      function GetStarttime : longint;
      function GetEndtime : longint;
      procedure StartTimer;
      procedure EndTimer;

    published
      property Id : string read FId write FId;
      property ArticleId : string read FArticleId write FArticleId;
      property Amount : longint read FAmount write FAmount;
      property Starttime : TSimulationTime read FStarttime;
      property Endtime : TSimulationTime read FEndtime;
      property EnterBufferTime : TSimulationTime read FEnterBufferTime write FEnterBufferTime;
  end;



  TOrderList = class(TList)
    private
    public
      constructor Create;
      destructor Destroy;
      function FindById(OrderId : string) : TOrder;
      procedure Reset;
      procedure Clear; override;
      function IsFinished : boolean;
    published
    end;


var
  OrderList : TOrderList;


implementation

uses SimController;

{ TOrderItem }

function TOrderItem.GetTotalTime: longint;
begin
  result:=FSetuptime+FAmount*FProcesstime;
end;

procedure TOrderItem.StartWorking;
begin
  if FStarttime.Time>0 then Exit; //order item is already running
  FStarttime.Assign(SimulationController.Time);
end;


procedure TOrderItem.DoNextStep;
begin
  if FOrderState=osFinished then exit;

  if FTime<GetTotalTime then
    begin
      FTime:=FTime+SimulationController.TimeInterval;
      SetOrderState(osInSetup);
      if FTime>FSetuptime then
        begin
          SetOrderState(osInProcess);
          FAmountFinished:=Round((FTime-FSetuptime)/FProcesstime);
        end;
    end
  else
    begin
      SetOrderState(osFinished);
      FEndtime.Assign(SimulationController.Time);
    end;
end;

procedure TOrderItem.SetOrderState(AValue: TOrderState);
begin
  if FOrderState=AValue then Exit;
  FOrderState:=AValue;
end;

constructor TOrderItem.Create;
begin
  inherited Create;
  FOrderState:=osUntouched;
  FStarttime:=TSimulationTime.Create;
  FEndtime:=TSimulationTime.Create;

  Reset;
end;

destructor TOrderItem.Destroy;
begin
  FStarttime.Free;
  FEndtime.Free;

  inherited Destroy;
end;

procedure TOrderItem.Reset;
begin
  FTime:=0;
  FAmountFinished:=0;
  FOrderState:=osUntouched;
  FStarttime.Reset;
  FEndtime.Reset;
end;



{ TOrder }

constructor TOrder.Create;
begin
  inherited Create;

  FStarttime:=TSimulationTime.Create;
  FEndtime:=TSimulationTime.Create;
  FEnterBufferTime:=TSimulationTime.Create;

  Reset;
end;

destructor TOrder.Destroy;
begin
  FStarttime.Free;
  FEndtime.Free;
  inherited Destroy;
end;

procedure TOrder.Reset;
begin
  FStarttime.Reset;
  FEndtime.Reset;
  FEnterBufferTime.Reset;
end;

function TOrder.GetNextOrderItem: TOrderItem;
var
  i : integer;
  OrderItem: TOrderItem;
begin
  result:=nil;
  for i:=0 to self.Count-1 do
    begin
      OrderItem:=TOrderItem(self[i]);
      if OrderItem.OrderState=osUntouched then
        begin
          result:=OrderItem;
          exit;
        end;
    end;

end;

function TOrder.GetCurrentWorkcenterId: string;
var
  i : integer;
  OrderItem: TOrderItem;
begin
  result:='';
  for i:=0 to self.Count-1 do
    begin
      OrderItem:=TOrderItem(self[i]);
      if OrderItem.OrderState<>osFinished then
        begin
          result:=OrderItem.WorkcenterId;
          exit;
        end;
    end;
end;

function TOrder.GetOrderState: TOrderState;
var
  i,j : integer;
  OrderItem: TOrderItem;
begin
  result:=osUntouched;
  j:=0;
  for i:=0 to self.Count-1 do
    begin
      OrderItem:=TOrderItem(self[i]);
      if OrderItem.OrderState=osFinished then j:=j+1;
    end;
  if j=self.Count then result:=osFinished
  else if j>0 then result:=osInProcess;

end;

function TOrder.GetStarttime: longint;
begin
  if self.Count>0 then
    result:=TOrderItem(self[0]).Starttime.Time
  else result:=0;
end;

function TOrder.GetEndtime: longint;
begin
  if self.Count>0 then
    result:=TOrderItem(self[self.Count-1]).Endtime.Time
  else result:=0;
end;

procedure TOrder.StartTimer;
begin
  if FStarttime.Time>0 then Exit; //order is already running
  FStarttime.Assign(SimulationController.Time);
end;

procedure TOrder.EndTimer;
begin
  if FEndtime.Time>0 then Exit; //order is already finished
  FEndtime.Assign(SimulationController.Time);
end;



{ TOrderList }

constructor TOrderList.Create;
begin
  inherited Create;
end;

destructor TOrderList.Destroy;
begin
  inherited Destroy;
end;

function TOrderList.FindById(OrderId: string): TOrder;
var
  i : integer;
  Order : TOrder;
begin
  result:=nil;
  for i:=0 to self.Count-1 do
    begin
      Order:=TOrder(self[i]);
      if Order.Id=OrderId then
        begin
          result:=Order;
          exit;
        end;
    end;
end;

procedure TOrderList.Reset;
var
  i : integer;
  Order : TOrder;
begin
  for i:=0 to self.Count-1 do
    begin
      Order:=TOrder(self[i]);
      Order.Reset;
    end;
end;

procedure TOrderList.Clear;
var
  i : integer;
  Order : TOrder;
begin
  for i:=0 to self.Count-1 do
    begin
      Order:=TOrder(self[i]);
      Order.Free;
    end;

  inherited Clear;
end;

function TOrderList.IsFinished: boolean;
var
  i : integer;
  Order : TOrder;
begin
  result:=true;
  for i:=0 to self.Count-1 do
    begin
      Order:=TOrder(self[i]);
      if Order.GetOrderState<>osFinished then
        begin
          result:=false;
          exit;
        end;
    end;
end;






initialization
  OrderList:=TOrderList.Create;


finalization
  OrderList.Free;


end.

