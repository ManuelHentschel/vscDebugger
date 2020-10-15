
import { DebugProtocol } from './debugProtocol';
import { REnvironment, RCall, RValue, RVector } from './RTypes';

export declare module StackTree {

  interface MinimalVariable {
    name: string;
    rValue: RValue;
    
    setter?: RValue;
    setInfo?: SetInfo;
  }

  interface Content {
    nodeType: string;
  }
  interface Stack extends Content {} // and DebugProtocol.StackTraceResponse.body
  interface Frame extends Content, DebugProtocol.StackFrame {}
  interface Scope extends Content, DebugProtocol.Scope {}
  interface Variable extends Content, DebugProtocol.Variable {}

  interface GetContentArgs {
    nodeType: string;
    lazy?: boolean; // indicates that no further computation should be done
  }
  interface GetStackArgs extends GetContentArgs {}
  interface GetFrameArgs extends GetContentArgs {}
  interface GetVariableArgs extends GetContentArgs {
    includeSetInfo?: boolean;
  }
  interface GetScopeArgs extends GetVariableArgs {}

  interface GetChildrenArgs {
    nodeType: string;
    refresh?: boolean; // indicates that the children should be computed new
    lazy?: boolean; // indicates that no further computation should be done
  }
  interface GetFramesArgs extends GetChildrenArgs {
    frameIdR?: number;
    frameId?: number;
  }
  interface GetVariablesArgs extends GetChildrenArgs {
    filter?: 'named' | 'indexed';
    start?: number;
    count?: number;
  }
  interface GetScopesArgs extends GetVariablesArgs {}

  interface InitializeArgs {
    nodeType: string;
  }
  interface StackArgs extends InitializeArgs {
    frameIdsR?: RVector<number>;
    topFrameId?: number;
    skipFromTop?: number;
    skipFromBottom?: number;
    forceDummyStack?: boolean;
    dummyFile?: string;
  }
  interface FrameArgs extends InitializeArgs {
    frameIdR: number;
    frameIdVsc: number;
    dummyFile?: string;
    isDummyFrame?: boolean;
  }
  interface VariableArgs extends InitializeArgs {
    name: string;
    rValue: RValue;
    setter?: RValue;
    setInfo?: SetInfo;
  }
  interface ScopeArgs  extends VariableArgs {}

  interface FindChildNodeArgs {
    findBy: string;
    varRef?: number;
    variablesReference?: number;
    name?: string;
  }

  interface SetInfo {
    setter?: RValue, // e.g. quote(v[1,]), sufficient for non-root elements
    expression?: RValue; //only required for root element
    environment?: REnvironment; // only required for root element
  }


  class Node {
    public getParent(): Node;
    public getAncestors(includeStartingNode?: boolean): Node[];
    public getChildren(args?: GetChildrenArgs): Node[];
    public getContent(args?: GetContentArgs): Content;
    public initialize(args?: InitializeArgs, parent?: Node): void;
    public findChildNode(args?: FindChildNodeArgs): Node|null;
    public getNewVarRef(): number;

    protected parent: Node;
    protected children: Node[];
    protected content?: Content;
    protected newVarRef: number;
    protected childrenVarRefs: number[];
  }

  class MetaNode extends Node {}

  class RootNode extends MetaNode {
    public getStackNode(args?: GetChildrenArgs): StackNode;
    public getEvalRootNode(args?: GetChildrenArgs): EvalRootNode;
  }

  class EvalRootNode extends MetaNode {
    public addChild(args: VariableArgs): VariableNode;
  }

  class StackNode extends Node {
    public totalFrames: number;

    public topFrameId: number;
    public skipFromTop: number;
    public skipFromBottom: number;
    public forceDummyStack: boolean;
    public dummyFile: string;
    public frameIdsR: RVector<number>;
    public frameIdsVsc: RVector<number>;

    public initialize(args: StackArgs): void;
    public getChildren(args: GetFramesArgs): FrameNode[];
    public getContent(args: GetStackArgs): Stack;

    protected parent: RootNode;
    protected children: FrameNode[];
  }

  class FrameNode extends Node {
    public id: number;
    public name: string;
    public source: object;
    public line: number;
    public column: number;
    public endLine: number;
    public endColumn: number;
    public instructionPointerReference: null;
    public moduleId: number;
    public presentationHint: 'normal' | 'label' | 'subtle';

    public frameIdR: number;
    public frameIdVsc: number;
    public dummyFile: string;
    public isDummyFrame: boolean;
    public firstenv: REnvironment;
    public lastenv: REnvironment;
    public call: RCall;

    public initialize(args: FrameArgs): void;
    public getChildren(args: GetScopesArgs): ScopeNode[];
    public getContent(args: GetFrameArgs): Frame;

    protected parent: StackNode;
    protected children: ScopeNode[];
  }

  class VariableNode extends Node {
    public name: string;
    public value: string;
    public type: string;
    public presentationHint: DebugProtocol.VariablePresentationHint;
    public evaluateName: string;
    public variablesReference: number;
    public namedVariables: number;
    public indexedVariables: number;
    public memoryReference: null;

    public rValue: RValue;
    public setter: RValue;
    public setInfo: SetInfo;
    public internalAttributes: MinimalVariable[];
    public customAttributes: MinimalVariable[];
    // public attrVars: MinimalVariable[];
    // public childVars: MinimalVariable[];

    public initialize(args: VariableArgs): void;
    public getChildren(args: GetVariablesArgs): VariableNode[];
    public getContent(args: GetVariableArgs): Variable | Scope;

    protected parent: ScopeNode | VariableNode | EvalRootNode;
    protected children: [];

    protected attrVars: {
      filter: 'named';
      index: number;
      minVar?: MinimalVariable;
      node?: VariableNode;
    }[];
    protected childVars: {
      filter: 'indexed';
      index: number;
      minVar?: MinimalVariable;
      node?: VariableNode;
    }[];
  }

  class ScopeNode extends VariableNode {
    public expensive: boolean;
    public source: object;
    public line: number;
    public column: number;
    public endLine: number;
    public endColumn: number;

    public initialize(args: ScopeArgs): void;
    public getChildren(args: GetVariablesArgs): VariableNode[];
    public getContent(args: GetScopeArgs): Scope;
  }
}
