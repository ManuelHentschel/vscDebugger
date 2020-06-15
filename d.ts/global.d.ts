

import { Breakpoints } from './breakpointManagement';
import { LazyTree } from './lazyTree';
import { RValue, REnvironment, RFunction, RCall, RNULL, RList, RVector } from './RTypes';

export declare module Session {
  interface Session {
    // settings:
    setBreakpointsInPackages: boolean;
    debugGlobal: boolean;
    breakOnErrorFromConsole: boolean;
    breakOnErrorFromFile: boolean;
    assignToAns: boolean;

    overwritePrint: boolean;
    overwriteCat: boolean;
    overwriteSource: boolean;

    rStrings: {
      delimiter0: string;
      delimiter1: string;
      prompt: string;
      continue: string;
      append: string;
    }

    threadId: number; //dummy

    // debugSession:
    noDebug: boolean; // todo
    debugMode: ("function" | "file" | "workspace")
    allowGlobalDebugging: boolean;
    workingDirectory: string;
    file: string;
    mainFunction: string;
    includePackages: boolean;

    // state:
    isInitialized: boolean;
    isConfigurationDone: boolean;
    isRunningDebugSession: boolean;
    isEvaluating: boolean;

    // data:
    tree?: LazyTree.LazyTree;
    stackNode: LazyTree.NodeId;
    frameIds: {
      vsc: number[];
      R: number[];
      node: number[]
    }
    varRefs: {
      varRef: number[];
      node: number[];
    }
    varRef: number;
    breakpointId: number;
    fileBreakpoints: Breakpoints.FileBreakpoints[];


    // (deprecated)
    varLists: any; // deprecated
    varListArgs: any; // deprecated
    varListPersistent: any; // deprecated

    frameIdsR: any; // deprecated
    frameIdsVsc: any; // deprecated
    breakpoints: any; // deprecated
    varInfos: any; // deprecated
    srcBreakpoints: any; // deprecated
  }
}
