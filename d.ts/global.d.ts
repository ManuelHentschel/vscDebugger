

import { Breakpoints } from './breakpoints';
import { RValue, REnvironment, RFunction, RCall, RNULL, RList, RVector } from './RTypes';
import { VarInfo } from './customVarInfo';
import { StackTree } from './stackTree';

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
    overwriteSource: boolean;

    noDebug: boolean; // currently not used
    debugMode: ("function" | "file" | "workspace")
    workingDirectory: string;
    file: string;
    mainFunction: string;
    includePackageScopes: boolean;
    setBreakpointsInPackages: boolean;
    packagesBeforeLaunch: string[];

    // server/communication:
    // (set for this debug session)
    // (should not influence the behaviour of the "R facing part" of the debugger)
    useJsonServer?: boolean;
    jsonPort?: number;
    jsonHost?: string;
    jsonServerConnection?: RValue; // only if useJsonServer==TRUE

    useSinkServer?: boolean;
    sinkPort?: number;
    sinkHost?: string;
    sinkServerConnection?: RValue; // only if useSinkServer==TRUE

    threadId: number; //dummy, but must match the one used in the DAP host

    rStrings: {
      delimiter0: string;
      delimiter1: string;
      prompt: string;
      continue: string;
    }

    // state:
    // (is managed by the debugger itself and might change frequently)
    breakOnErrorFromConsole: boolean;
    breakOnErrorFromFile: boolean;
    isInitialized: boolean;
    isConfigurationDone: boolean;
    isEvaluating: boolean;
    isError: boolean;
    entryFrames: number[];
    launchFrames: number;
    ignoreNextCallback: boolean;
    breakpointId: number;

    // data:
    // (like 'state', but contains longer lists etc.)
    rootNode: StackTree.RootNode;
    fileBreakpoints: Breakpoints.FileBreakpoints[];
  }
}
