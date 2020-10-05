

import { Breakpoints } from './breakpoints';
import { RValue, REnvironment, RFunction, RCall, RNULL, RList, RVector } from './RTypes';
import { VarInfo } from './customVarInfo';
import { StackTree } from './stackTree';
import { DebugProtocol } from './debugProtocol';
import { Source } from './debugProtocolModifications';

export declare module Session {
  interface Session {
    // settings:
    // (usually changed globally, persisting across debug sessions)
    varInfos: VarInfo[];

    // debugSession:
    // (set for this debug session)
    allowGlobalDebugging: boolean;
    overwritePrint: boolean;
    overwriteCat: boolean;
    overwriteMessage: boolean;
    overwriteSource: boolean;

    noDebug: boolean; 
    debugMode: ("function" | "file" | "workspace")
    workingDirectory: string;
    file: string;
    mainFunction: string;
    includePackageScopes: boolean;
    setBreakpointsInPackages: boolean;
    debuggedPackages: string[];

    // server/communication:
    // (set for this debug session)
    // (should not influence the behaviour of the "R facing part" of the debugger)
    jsonPort?: number;
    jsonHost?: string;
    jsonServerConnection?: RValue;

    sinkPort?: number;
    sinkHost?: string;
    sinkServerConnection?: RValue;

    threadId: number; //dummy, but must match the one used in the DAP host

    rStrings: {
      prompt: string;
      continue: string;
    }

    // state:
    // (is managed by the debugger itself and might change frequently)
    breakOnErrorFromConsole: boolean;
    breakOnErrorFromFile: boolean;
    entryFrames: number[];
    launchFrames: number[];
    breakpointId: number;
    stopListeningOnPort: boolean;
    restOfLine: string;

    state: State;
    pendingEvalResponses: DebugProtocol.EvaluateResponse[];

    // data:
    // (like 'state', but contains longer lists etc.)
    rootNode: StackTree.RootNode;
    fileBreakpoints: Breakpoints.FileBreakpoints[];
    sources: Source[];
  }
}

export type BaseState = "starting"|"loadLib"|"sourceMain"|"runMain"|"runFile"|"workspace"|"quitting";
export type RunningWhat = "loadLib"|"sourceMain"|"file"|"main"|"eval";
export type PausedOn = "breakpoint"|"browser"|"error"|"toplevel";

export class MinimalState {
  baseState: ""|BaseState;
  running: boolean;
  runningWhat: ""|RunningWhat;
  evalSilent: boolean;
  pausedOn: ""|PausedOn;
  hasHitError: boolean;
}

export class State extends MinimalState {
  update(
    changeBaseState?: boolean,
    baseState?: BaseState,
    startRunning?: boolean,
    runningWhat?: RunningWhat,
    evalSilent?: boolean,
    startPaused?: boolean,
    pausedOn?: PausedOn
  ): MinimalState;

  changeBaseState(baseState: BaseState, startRunning?: boolean, startPaused?: boolean): MinimalState;
  startRunning(runningWhat: RunningWhat, evalSilent?: boolean): MinimalState;
  startPaused(pausedOn: PausedOn): MinimalState;

  export(): MinimalState;
  revert(state: MinimalState): MinimalState;

  isError(): boolean;
  isRunning(): boolean;
  isRunningFile(): boolean;
  isSourcingMain(): boolean;
  isRunningMain(): boolean;
  isRunningFileOrMain(): boolean;
  isEvaluating(): boolean;
  isEvaluatingSilent(): boolean;
  isPaused(): boolean;
}
