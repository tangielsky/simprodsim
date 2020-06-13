unit SimObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;


type

  TOrderState = (osUntouched,osInSetup,osInProcess,osFinished);
  TWorkcenterStateDef = (wcsWait,wcsSetup,wcsProcess); //wcsBreakdown, wcsBlocked

  TWorkcenterStateProps = record
    State : TWorkcenterStateDef;
    Caption : string;
    Forecolor : TColor;
    Backcolor : TColor;
  end;



var
  WorkcenterStateWait,WorkcenterStateProcess,WorkcenterStateSetup : TWorkcenterStateProps;

function GetWorkcenterStateProps(AState : TWorkcenterStateDef) : TWorkcenterStateProps;

implementation


function GetWorkcenterStateProps(AState : TWorkcenterStateDef) : TWorkcenterStateProps;
begin
  if AState=wcsSetup then result:=WorkcenterStateSetup
  else if AState=wcsProcess then result:=WorkcenterStateProcess
  else result:=WorkcenterStateWait;
end;


initialization

  WorkcenterStateWait.State:=wcsWait;
  WorkcenterStateWait.Caption:='Wait';

  WorkcenterStateSetup.State:=wcsSetup;
  WorkcenterStateSetup.Caption:='Setup';

  WorkcenterStateProcess.State:=wcsProcess;
  WorkcenterStateProcess.Caption:='Process';


finalization



end.

