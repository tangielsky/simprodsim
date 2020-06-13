unit SimWorkcenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Graphics, ComCtrls, Controls, Forms,
  SimPieChart,
  SimTime,
  SimObjects,
  SimOrder,
  SimBuffer;

type
  TWorkcenter = class(TPanel)
    private
      FActiveOrder : TOrder;
      FActiveOrderItem : TOrderItem;
      FCapacityPerDay : longint;
      FCaption : String;
      FId : String;
      FImagePath : string;
      FInputBuffer : TSimulationBuffer;
      FMoveable : boolean;
      FOutputBuffer : TSimulationBuffer;
      FProcesstime : TSimulationTime;
      FSetuptime : TSimulationTime;
      FState : TWorkcenterStateDef;
      FView : TWinControl;
      FWaittime : TSimulationTime;

      RelPosX,RelPosY: Integer;

      //Form elements
      LabelId : TLabel;
      LabelCaption : TLabel;
      LabelOrder : TLabel;
      LabelAmount : TLabel;
      ListboxInput : TListbox;
      ListboxOutput : TListbox;
      SimPieChartOrderTime : TSimPieChart;

      procedure HeadMouseMove(Sender: TObject; Shift: TShiftState; X,
        Y: Integer);
      procedure HeadMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X,Y: Integer);
      procedure SetActiveOrder(AValue: TOrder);
      procedure SetId(AValue : string);
      procedure SetCaption(AValue: string);
      procedure SetMoveable(AValue: boolean);
      procedure SetState(AValue: TWorkcenterStateDef);
      procedure SetView(AValue: TWinControl);
    public
      SimPieChartState : TSimPieChart;
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy;
      procedure DoNextStep;
      function ForwardOrder: TOrder;
      procedure Reset;
      procedure UpdateLabels;
      procedure UpdateView;
    published
      property ActiveOrder : TOrder read FActiveOrder write SetActiveOrder;
      property CapacityPerDay : longint read FCapacityPerDay write FCapacityPerDay;
      property Caption : string read FCaption write SetCaption;
      property Id : string read FId write SetId;
      property ImagePath : string read FImagePath write FImagePath;
      property InputBuffer : TSimulationBuffer read FInputBuffer write FInputBuffer;
      property Moveable : boolean read FMoveable write SetMoveable;
      property OutputBuffer : TSimulationBuffer read FOutputBuffer write FOutputBuffer;
      property Runtime : TSimulationTime read FProcesstime;
      property Setuptime : TSimulationTime read FSetuptime;
      property State : TWorkcenterStateDef read FState write SetState;
      property View : TWinControl read FView write SetView;
      property Waittime : TSimulationTime read FWaittime;
  end;

  TWorkcenterList = class(TList)
    private
    public
      constructor Create;
      destructor Destroy;
      function FindById(WorkcenterId: string): TWorkcenter;
      procedure Clear;
    published
  end;



var
  WorkcenterList : TWorkcenterList;


implementation


uses SimController;


{ TWorkcenter }

procedure TWorkcenter.SetId(AValue: string);
begin
  if FId=AValue then Exit;
  FId:=AValue;
  LabelId.Caption:=FId;
end;

procedure TWorkcenter.SetCaption(AValue: string);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  LabelCaption.Caption:=FCaption;
end;

procedure TWorkcenter.SetMoveable(AValue: boolean);
begin
  if FMoveable=AValue then Exit;
  FMoveable:=AValue;

  if FMoveable then
    begin
      LabelId.OnMouseMove:=@HeadMouseMove;
      LabelCaption.OnMouseMove:=@HeadMouseMove;
    end
  else
    begin
      LabelId.OnMouseMove:=nil;
      LabelCaption.OnMouseMove:=nil;
    end;
end;

procedure TWorkcenter.SetState(AValue: TWorkcenterStateDef);
var wsp : TWorkcenterStateProps;
begin
  if FState=AValue then exit;

  FState:=AValue;

  wsp:=GetWorkcenterStateProps(FState);
  LabelId.Font.Color:=wsp.Forecolor;
  LabelId.Color:=wsp.Backcolor;
  LabelCaption.Font.Color:=wsp.Forecolor;
  LabelCaption.Color:=wsp.Backcolor;
end;

procedure TWorkcenter.SetView(AValue: TWinControl);
begin
  if FView=AValue then Exit;
  FView:=AValue;
  UpdateView;
end;


procedure TWorkcenter.SetActiveOrder(AValue: TOrder);
begin
  //if FActiveOrder=AValue then Exit;
  FActiveOrder:=AValue;
  if FActiveOrder=nil then FActiveOrderItem:=nil
  else FActiveOrderItem:=FActiveOrder.GetNextOrderItem;


  if FActiveOrder=nil then LabelOrder.Caption:='-'
  else LabelOrder.Caption:=FActiveOrder.Id;

  if FActiveOrderItem=nil then LabelAmount.Caption:='-'
  else LabelAmount.Caption:=IntToStr(FActiveOrderItem.AmountFinished)+'/'+IntToStr(FActiveOrderItem.Amount);

  if FActiveOrder=nil then SetState(wcsWait);
  //UpdateLabels;
end;


constructor TWorkcenter.Create(TheOwner: TComponent);
var x,y,a : integer;

  procedure CreateLabel(var l : TLabel; al : TAlign; am : TAlignment; bold,transparent : boolean; x,y : integer);
  begin
    l:=TLabel.Create(self);
    l.Align:=al;
    l.Top:=y;
    l.Left:=x;
    l.Alignment:=am;
    l.Transparent:=transparent;
    l.ParentFont:=true;
    l.Font.Bold:=bold;
    InsertControl(l);
  end;
  procedure CreateLabel2(s : string; x,y : integer);
  var l : TLabel;
  begin
    l:=TLabel.Create(self);
    l.Top:=10+y;
    l.Left:=x;
    l.Caption:=s;
    l.ParentFont:=true;
    InsertControl(l);
  end;
  procedure CreateLabel1(var l : TLabel; x,y : integer);
  begin
    l:=TLabel.Create(self);
    l.Top:=10+y;
    l.Left:=x;
    l.Caption:='';
    l.ParentFont:=true;
    InsertControl(l);
  end;


begin
  inherited Create(TheOwner);

  FInputBuffer:=TSimulationBuffer.Create;
  FOutputBuffer:=TSimulationBuffer.Create;

  FWaittime:=TSimulationTime.Create;
  FSetuptime:=TSimulationTime.Create;
  FProcesstime:=TSimulationTime.Create;


  Height:=220;
  Width:=190;
  x:=105;
  y:=28;
  a:=15;

  CreateLabel(LabelCaption,alTop,taCenter,false,false,0,0);
  CreateLabel(LabelId,alTop,taCenter,true,false,0,0);

  LabelId.OnMouseDown:=@HeadMouseDown;
  LabelCaption.OnMouseDown:=@HeadMouseDown;


  SetMoveable(true);

  Color:=clForm;

  CreateLabel2('Order',5,y);
  CreateLabel1(LabelOrder,x,y+a*0);

  CreateLabel2('Amount',5,y+a*1);
  CreateLabel1(LabelAmount,x,y+a*1);


  SimPieChartState:=TSimPieChart.Create(self);
  InsertControl(SimPieChartState);
  SimPieChartState.Left:=5;
  SimPieChartState.Top:=y+a*3-1;
  SimPieChartState.Width:=a*4;
  SimPieChartState.Height:=a*4;
  SimPieChartState.ShowText:=true;
  SimPieChartState.ShowTextStyle:=spcPercentage;
  SimPieChartState.AddValue(0,WorkcenterStateProcess.Caption,'s',WorkcenterStateProcess.Backcolor);
  SimPieChartState.AddValue(1,WorkcenterStateSetup.Caption,'s',WorkcenterStateSetup.Backcolor);
  SimPieChartState.AddValue(2,WorkcenterStateWait.Caption,'s',WorkcenterStateWait.Backcolor);


  SimPieChartOrderTime:=TSimPieChart.Create(self);
  InsertControl(SimPieChartOrderTime);
  SimPieChartOrderTime.Left:=5+a*4+5;
  SimPieChartOrderTime.Top:=y+a*3-1;
  SimPieChartOrderTime.Width:=a*4;
  SimPieChartOrderTime.Height:=a*4;
  SimPieChartOrderTime.AddValue(0,'Ordertime','s',clYellow);
  SimPieChartOrderTime.AddValue(1,'rest','s',clGray);


  ListboxInput:=TListbox.Create(self);
  ListboxInput.Top:=y+a*8;
  ListboxInput.Left:=5;
  ListboxInput.Width:=(Width div 2)-5-3;
  ListboxInput.Height:=Height-ListboxInput.Top-ListboxInput.Left;
  ListboxInput.ParentFont:=true;
  InsertControl(ListboxInput);

  ListboxOutput:=TListbox.Create(self);
  ListboxOutput.Top:=y+a*8;
  ListboxOutput.Left:=ListboxInput.Left*2+ListboxInput.Width;
  ListboxOutput.Width:=ListboxInput.Width;
  ListboxOutput.Height:=Height-ListboxInput.Top-ListboxInput.Left;
  ListboxOutput.ParentFont:=true;
  InsertControl(ListboxOutput);

  CreateLabel2('Input',5,y+a*6+5);
  CreateLabel2('Output',ListboxOutput.Left,y+a*6+5);

  FInputBuffer.View:=ListboxInput;
  FOutputBuffer.View:=ListboxOutput;

  Reset;

end;

destructor TWorkcenter.Destroy;
begin
  InputBuffer.Free;
  OutputBuffer.Free;

  FWaittime.Free;
  FSetuptime.Free;
  FProcesstime.Free;

  inherited Destroy;
end;

procedure TWorkcenter.HeadMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  RelPosX:=X; RelPosY:=Y;
  UpdateView;
end;

procedure TWorkcenter.HeadMouseMove(Sender: TObject;Shift: TShiftState; X,Y: Integer);
var xnew,ynew : integer;
begin
  if (ssLeft in Shift) and (FMoveable=true) then
    begin
      xnew:=Left+X-RelPosX;
      if xnew<0 then xnew:=0;
      ynew:=Top+Y-RelPosY;
      if ynew<0 then ynew:=0;
      SetBounds(xnew,ynew,Width,Height);
    end;
end;

procedure TWorkcenter.UpdateLabels;
var
  i : integer;
  Order : TOrder;
begin
  //if SimulationController.Time.Second mod 20<>0 then exit;

  if FActiveOrderItem=nil then LabelAmount.Caption:='-'
  else LabelAmount.Caption:=IntToStr(FActiveOrderItem.AmountFinished)+'/'+IntToStr(FActiveOrderItem.Amount);

  SimPieChartState.EditValue(0,FProcesstime.Time);
  SimPieChartState.EditValue(1,FSetuptime.Time);
  SimPieChartState.EditValue(2,FWaittime.Time);
  SimPieChartState.Repaint;

  if FActiveOrderItem=nil then
    begin
      SimPieChartOrderTime.EditValue(0,0);
      SimPieChartOrderTime.EditValue(1,100);
      SimPieChartOrderTime.Repaint;
    end
  else
    begin
      SimPieChartOrderTime.EditValue(0,FActiveOrderItem.Time);
      SimPieChartOrderTime.EditValue(1,FActiveOrderItem.GetTotalTime-FActiveOrderItem.Time);
      SimPieChartOrderTime.Repaint;
    end;

end;

procedure TWorkcenter.UpdateView;
var
  Listview : TListview;
  s1,s2,s3 : string;

  procedure AddItem(ACaption : string; AValue : string);
  var
    ListItem : TListItem;
  begin
    ListItem:=Listview.Items.Add;
    ListItem.Caption:=ACaption;
    ListItem.SubItems.Add(AValue);
  end;

begin
  if FView=nil then exit;

  if FView is TListView then
    begin
      Listview:=FView as TListView;
      Listview.BeginUpdate;
      Listview.Items.Clear;
      AddItem('Id',FId);
      AddItem('Caption',FCaption);
      if FActiveOrder=nil then
        begin
          s1:='';
          s2:='';
          s3:='';
        end
      else
        begin
          s1:=FActiveOrder.Id;
          s2:=FActiveOrder.ArticleId;
          s3:=IntToStr(FActiveOrder.Amount);
        end;
      AddItem('Order',s1);
      AddItem('Article',s2);
      AddItem('Amount',s3);
      AddItem('Waittime',FWaittime.FormatStrHM);
      AddItem('Setuptime',FSetuptime.FormatStrHM);
      AddItem('Processtime',FProcesstime.FormatStrHM);
      AddItem('Input max. orders',IntToStr(FInputbuffer.ResultOrdersMax));
      AddItem('Input max. amount',IntToStr(FInputbuffer.ResultAmountMax));
      AddItem('Output max. orders',IntToStr(FOutputbuffer.ResultOrdersMax));
      AddItem('Output max. amount',IntToStr(FOutputbuffer.ResultAmountMax));
      Listview.EndUpdate;
    end;
end;


procedure TWorkcenter.Reset;
begin
  SetActiveOrder(nil);
end;


procedure TWorkcenter.DoNextStep;
var
  NextOrderItem : TOrderItem;
begin

  (*
  Workcenter works alone til order change:
  - the next operation has the same workcenter
  - the outputbuffer is free
  - the inputbuffer is not empty
  *)

  if FActiveOrder=nil then
    begin
      SetState(wcsWait);

      if InputBuffer.HasOrders then
        begin
          SetActiveOrder(FInputBuffer.ForwardOrder(0));
          FActiveOrderItem.StartWorking;
        end;
    end;

  if FActiveOrder<>nil then
    begin
      FActiveOrderItem.DoNextStep;
      if FActiveOrderItem.OrderState=osFinished then
        begin
          SetState(wcsWait);

          //is the next operation the same workcenter?
          NextOrderItem:=FActiveOrder.GetNextOrderItem;
          if (NextOrderItem<>nil) and (NextOrderItem.WorkcenterId=FId) then
            begin
              //if same workcenter, then start operation
              FActiveOrderItem:=NextOrderItem;
              FActiveOrderItem.StartWorking;
            end
          else
            begin
              if FOutputBuffer.IsFree then
                begin
                  FOutputBuffer.AddOrder(FActiveOrder);
                  SetActiveOrder(nil);
                end;
            end;

        end
      else
        begin
          if FActiveOrderItem.State=osInSetup then SetState(wcsSetup)
          else if FActiveOrderItem.State=osInProcess then SetState(wcsProcess);
        end;
    end;


  if FState=wcsSetup then FSetuptime.Inc
  else if FState=wcsProcess then FProcesstime.Inc
  else FWaittime.Inc;

end;

function TWorkcenter.ForwardOrder : TOrder;
var
  Order : TOrder;
begin
  result:=FActiveOrder;
  SetActiveOrder(nil);
  SetState(wcsWait);
end;




{ TWorkcenterList }

constructor TWorkcenterList.Create;
begin
  inherited Create;
end;

destructor TWorkcenterList.Destroy;
begin
  inherited Destroy;
end;

function TWorkcenterList.FindById(WorkcenterId: string): TWorkcenter;
var
  i : integer;
  Workcenter : TWorkcenter;
begin
  result:=nil;
  for i:=0 to self.Count-1 do
    begin
      Workcenter:=TWorkcenter(self[i]);
      if Workcenter.Id=WorkcenterId then
        begin
          result:=Workcenter;
          exit;
        end;
    end;
end;


procedure TWorkcenterList.Clear;
var
  i : integer;
  Workcenter : TWorkcenter;
begin
  for i:=0 to self.Count-1 do
    begin
      Workcenter:=TWorkcenter(self[i]);
      Workcenter.Free;
    end;

  inherited Clear;
end;




initialization
  WorkcenterList:=TWorkcenterList.Create;


finalization
  WorkcenterList.Free;


end.

