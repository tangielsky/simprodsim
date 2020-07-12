unit SimBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, Dialogs,
  SimOrder, SimObjects;

type

  { TSimulationBuffer }

  TSimulationBuffer = class
  private
    FList : TList;
    FMaxOrders : longint;
    FMinStayInTime : longint;
    FResultAmountMax : longint;
    FResultOrdersMax : longint;
    FView : TWinControl;

    procedure SetMaxOrders(AValue: longint);
    procedure SetView(AValue: TWinControl);

  public
    SimDataBufferOrders : TSimulationData;

    constructor Create;
    destructor Destroy;
    function FindByOrderId(OrderId: string): boolean;
    function Amount: longint;
    function IsFree: boolean;
    function HasOrders: boolean;
    function IsOrderAllowed(Index: integer): boolean;
    function ForwardOrder(Index : integer): TOrder;
    function GetOrder(Index : integer) : TOrder;
    procedure AddOrder(Order : TOrder);
    procedure Reset;
    procedure UpdateView;

  published
    property Orders : TList read FList write FList;
    property MaxOrders : longint read FMaxOrders write SetMaxOrders;
    property MinStayInTime : longint read FMinStayInTime write FMinStayInTime;
    property ResultAmountMax : longint read FResultAmountMax;
    property ResultOrdersMax : longint read FResultOrdersMax;
    property View : TWinControl read FView write SetView;
  end;


implementation

uses SimTime,SimController;


procedure TSimulationBuffer.SetView(AValue: TWinControl);
begin
  if FView=AValue then Exit;
  FView:=AValue;
  UpdateView;
end;

procedure TSimulationBuffer.SetMaxOrders(AValue: longint);
begin
  if FMaxOrders=AValue then Exit;
  FMaxOrders:=AValue;
  UpdateView;
end;

constructor TSimulationBuffer.Create;
begin
  inherited Create;

  SimDataBufferOrders:=TSimulationData.Create;

  FList:=TList.Create;
  FMaxOrders:=-1; //endless buffer
  FMinStayInTime:=0;
  FResultAmountMax:=0;
  FResultOrdersMax:=0;
  FView:=nil;
end;

destructor TSimulationBuffer.Destroy;
begin
  SimDataBufferOrders.Free;
  FList.Free;
  inherited Destroy;
end;

function TSimulationBuffer.FindByOrderId(OrderId: string): boolean;
var
  i : integer;
  Order : TOrder;
begin
  result:=false;
  for i:=0 to FList.Count-1 do
    begin
      Order:=TOrder(FList[i]);
      if Order.Id=OrderId then
        begin
          result:=true;
          exit;
        end;
    end;
end;


procedure TSimulationBuffer.UpdateView;
var
  i,j,k,found : integer;
  Listbox : TListbox;
  Listview : TListview;
  ListItem : TListItem;
  Order : TOrder;
  lstart,lend : longint;

begin
  if FView=nil then exit;

  SimDataBufferOrders.AddValue(FList.Count);

  if FView is TListview then
    begin

      Listview:=FView as TListview;
      Listview.BeginUpdate;
      //Delete one
      for j:=0 to Listview.Items.Count-1 do
        begin
          found:=-1;
          for i:=0 to FList.Count-1 do
            begin
              Order:=TOrder(FList[i]);
              if Order.Id=Listview.Items[j].Caption then
                begin
                  found:=i;
                  break;
                end;
              end;
          if found=-1 then
            begin
              Listview.Items.Delete(j);
              break;
            end;
        end;


      //Update or add one
      for i:=0 to FList.Count-1 do
        begin
          Order:=TOrder(FList[i]);
          found:=-1;
          for j:=0 to Listview.Items.Count-1 do
            if Listview.Items[j].Caption=Order.Id then
              begin
                found:=j;
                ListItem:=Listview.Items[j];
                break;
              end;
          if found=-1 then
            begin
              ListItem:=Listview.Items.Add;
              ListItem.Caption:=Order.Id;
              ListItem.SubItems.Add('');
              ListItem.SubItems.Add('');
              ListItem.SubItems.Add('');
            end;
          if ListItem<>nil then
            begin
              if Order.GetOrderState=osInProcess then ListItem.ImageIndex:=1
              else if Order.GetOrderState=osFinished then ListItem.ImageIndex:=2
              else ListItem.ImageIndex:=0;

              lstart:=Order.GetStarttime;
              lend:=Order.GetEndtime;

              for k:=1 to Listview.Columns.Count-1 do
                begin
                if Listview.Column[k].Caption='Start' then
                  ListItem.SubItems[k-1]:=TSimulationTime.FormatHM(lstart)
                else if Listview.Column[k].Caption='End' then
                  ListItem.SubItems[k-1]:=TSimulationTime.FormatHM(lend)
                else if Listview.Column[k].Caption='Leadtime' then
                  ListItem.SubItems[k-1]:=TSimulationTime.FormatHM(lend-lstart)
                else if Listview.Column[k].Caption='Amount' then
                  ListItem.SubItems[k-1]:=IntToStr(Order.Amount)
                else if Listview.Column[k].Caption='Article' then
                  ListItem.SubItems[k-1]:=Order.ArticleId;
               end;
            end;

        end;
      Listview.EndUpdate;

    end
  else if FView is TListbox then
    begin
      Listbox:=FView as TListbox;
      if FMaxOrders=0 then
        begin
          Listbox.Visible:=false;
          exit;
        end
      else Listbox.Visible:=true;

      Listbox.Items.Clear;
      Listbox.Items.Add('MAX: '+IntToStr(FResultOrdersMax)+'/'+IntToStr(FResultAmountMax));
      for i:=0 to FList.Count-1 do
        begin
          Order:=GetOrder(i);
          Listbox.Items.Add(Order.Id+' - '+IntToStr(Order.Amount)+' pcs');
        end;
    end;
end;

function TSimulationBuffer.Amount: longint;
var
  i : integer;
  a : longint;
  Order : TOrder;
begin
  a:=0;
  for i:=0 to FList.Count-1 do
    begin
      Order:=TOrder(FList[i]);
      a:=a+Order.Amount;
    end;
  result:=a;
end;


function TSimulationBuffer.HasOrders : boolean;
var
  Order : TOrder;
begin
  result:=false;
  if FList.Count>0 then
    begin
      Order:=TOrder(FList[0]);
      if SimulationController.Time.Time-Order.EnterBufferTime.Time>=FMinStayInTime then result:=true;
    end;
end;

function TSimulationBuffer.IsOrderAllowed(Index : integer) : boolean;
var
  i : integer;
  Order : TOrder;
begin
  result:=false;
  if FList.Count>Index then
    begin
      Order:=TOrder(FList[Index]);
      if SimulationController.Time.Time-Order.EnterBufferTime.Time>=FMinStayInTime then result:=true;
    end;
end;



procedure TSimulationBuffer.AddOrder(Order: TOrder);
var a : longint;
begin
  if Order=nil then exit;

  FList.Add(Order);
  Order.EnterBufferTime.Assign(SimulationController.Time);
  a:=Amount;
  if a>FResultAmountMax then FResultAmountMax:=a;
  if FList.Count>FResultOrdersMax then FResultOrdersMax:=FList.Count;

  UpdateView;
end;

procedure TSimulationBuffer.Reset;
begin
  SimDataBufferOrders.Reset;
  FList.Clear;
end;


function TSimulationBuffer.ForwardOrder(Index : integer) : TOrder;
var
  Order : TOrder;
begin
  result:=nil;
  if FList.Count>Index then
    begin
      Order:=TOrder(FList[Index]);
      result:=Order;
      FList.Delete(Index);
      UpdateView;
    end;

end;

function TSimulationBuffer.GetOrder(Index: integer): TOrder;
begin
  if FList.Count>Index then result:=TOrder(FList[Index])
  else result:=nil;
end;

function TSimulationBuffer.IsFree: boolean;
begin
  result:=false;
  if FMaxOrders<0 then result:=true
  else
    begin
      if FMaxOrders>FList.Count then result:=true;
    end;
end;



end.

