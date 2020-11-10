

import { Breakpoints } from './breakpoints';
import { RValue, REnvironment, RFunction, RCall, RNULL, RList, RVector } from './RTypes';
import { VarInfo } from './customVarInfo';
import { StackTree } from './stackTree';
import { DebugProtocol } from './debugProtocol';


interface InternalSource extends DebugProtocol.Source {
    content?: string;
}

export interface Session {
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
  splitOverwrittenOutput: boolean;

  supportsInvalidatedEvent: boolean;
  noDebug: boolean; 
  debugMode: ("function" | "file" | "workspace" | "attached" | "")
  workingDirectory: string;
  file: string;
  mainFunction: string;
  includePackageScopes: boolean;
  setBreakpointsInPackages: boolean;
  debuggedPackages: RVector<string>;
  loadPackages: RVector<string>;
  assignToAns: boolean;

  previousOptions?: {
    prompt?: string;
    continue?: string;
    browserNLdisabled?: boolean;
    error?: RFunction;
    // ...
    [key: string]: any;
  }
  internalOptions?: {
    prompt?: string;
    continue?: string;
    browserNLdisabled?: boolean;
    error?: RFunction;
    // ...
    [key: string]: any;
  }

  pid: number;
  ppid: number;
  terminalId: string;

  // server/communication:
  // (set for this debug session)
  // (should not influence the behaviour of the "R facing part" of the debugger)

  useDapSocket: boolean;
  dapPort?: number;
  dapHost?: string;
  dapSocketConnection?: RValue;

  useJsonSocket: boolean;
  jsonPort?: number;
  jsonHost?: string;
  jsonSocketConnection?: RValue;

  useSinkSocket: boolean;
  sinkPort?: number;
  sinkHost?: string;
  sinkSocketConnection?: RValue;
  sinkNumber: number; //=0

  useCustomSocket: boolean;
  customPort?: number;
  customHost?: string;
  customSocketConnection?: RValue;

  threadId: number; //dummy, but must match the one used in the DAP client

  rStrings: {
    prompt?: string;
    continue?: string;
    packageName: 'vscDebugger';
    attachName: 'tools:vscDebugger';
  }

  // custom events/requests:
  supportsWriteToStdinEvent?: boolean;
  supportsShowingPromptRequest?: boolean;
  supportsStdoutReading?: boolean;

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
  sourceBreakpointsList: Breakpoints.SourceBreakpoints[];
  sources: InternalSource[];
  breakpointEnvironments: REnvironment[];
}

export type BaseState = "starting"|"loadLib"|"sourceMain"|"runMain"|"runFile"|"workspace"|"quitting"|"attached"|"detached";
export type RunningWhat = "loadLib"|"sourceMain"|"file"|"main"|"eval"|"attachedCode";
export type PausedOn = "breakpoint"|"browser"|"error"|"toplevel"|"entry"|"step"|"pause";

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
  isPausedOnBreakpoint(): boolean;
  isPausedOnError(): boolean;
  isPausedAfterError(): boolean;
  isStarted(): boolean;
}
