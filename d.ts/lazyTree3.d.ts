
import { RValue, RFunction, RCall, RNULL, RList, RVector } from './RTypes';
import { DebugProtocol } from './DebugProtocol';


export declare module Tree {

  // helpers
  class REnvironment {}
	interface SetInfo {
		setter?: RValue, // e.g. quote(v[1,]), sufficient for non-root elements
		expression?: RValue; //only required for root element
		environment?: REnvironment; // only required for root element
  }
  
	type NodeId = number;
	enum NodeType {
		Meta = "Meta",
		Stack = "Stack",
		Frame = "Frame",
		Scope = "Scope",
		Variable = "Variable"
	}

	type ChildNodeType<T extends NodeType> = (
		T extends NodeType.Stack ? NodeType.Frame :
		T extends NodeType.Frame ? NodeType.Scope :
		T extends NodeType.Scope ? NodeType.Variable :
		T extends NodeType.Variable ? NodeType.Variable :
		NodeType
	)

	type ParentNodeType<T extends NodeType> = (
		T extends NodeType.Stack ? NodeType.Meta :
		T extends NodeType.Frame ? NodeType.Stack :
		T extends NodeType.Scope ? NodeType.Scope :
		T extends NodeType.Variable ? (NodeType.Variable | NodeType.Scope | NodeType.Meta) :
		NodeType
	)

  // makeNodeArgs
  interface MakeNodeArgs {
    parent: Node;
    nodeType: NodeType;
  }
  interface MakeMetaArgs extends MakeNodeArgs {
		nodeType: NodeType.Meta;
  }
	interface MakeStackArgs extends MakeNodeArgs {
		nodeType: NodeType.Stack;
		topFrameId: number;
		skipFromTop?: number;
		skipFromBottom?: number;
		isError: boolean;
		forceDummystack?: boolean;
		dummyFile?: string;
	}
	interface MakeFrameArgs extends MakeNodeArgs {
		nodeType: NodeType.Frame;
		frameIdR: number;
		frameIdVsc: number;
		dummyFile?: string; // determine in R?
		isDummyFrame?: boolean; // determine in R?
	}
	interface MakeScopeArgs extends MakeNodeArgs {
		nodeType: NodeType.Scope;
		name: string;
		rValue: RValue;
		setter?: RValue;
		setInfo?: SetInfo;
	}
	interface MakeVariableArgs extends MakeNodeArgs {
		nodeType: NodeType.Variable;
		name: string;
		rValue: RValue;
		setter?: RValue;
		setInfo?: SetInfo;
	}
	type GenericMakeContentArgs<T extends NodeType> = (
		T extends NodeType.Stack ? MakeStackArgs :
		T extends NodeType.Scope ? MakeScopeArgs :
		T extends NodeType.Frame ? MakeFrameArgs :
		T extends NodeType.Variable ? MakeVariableArgs :
		MakeNodeArgs
  )
  
  interface Content {
    nodeType: NodeType;
  }
	interface Variable extends Content, DebugProtocol.Variable, MakeVariableArgs {
		nodeType: NodeType.Variable;

		name: string;
		value: string;
		type: string;
		evaluateName: string;
		
		setter?: RValue;
		setInfo?: SetInfo;
		
    rValue: RValue;
		
		variablesReference: number;
	}
	interface Scope extends Content, DebugProtocol.Scope {
		nodeType: NodeType.Scope;
		name: string;
		rValue: REnvironment;
		variablesReference: number;
		setInfo?: SetInfo; // sensible here?
	}
	interface Frame extends Content, DebugProtocol.StackFrame {
		nodeType: NodeType.Frame;
		variablesReference: number;
		frameIdR: number;
	}
	interface Stack extends Content {
		nodeType: NodeType.Stack;
		totalFrames?: number;
	}
	type GenericContent<T extends NodeType> = (
		T extends NodeType.Stack ? Stack :
		T extends NodeType.Scope ? Scope :
		T extends NodeType.Frame ? Frame :
		T extends NodeType.Variable ? Variable :
		Content
  )
  

  
	


  interface GetChildrenArgs {}
  interface GetContentArgs {}
  interface Content {}

  class Node extends REnvironment {

    nodeType: NodeType;

    private content: Content;
    private children: Node[];
    private parent: Node;

    private childrenIndices?;



    getParent(): Node;
    getAncestors(): Node[];

    constructor(args: MakeNodeArgs);

    getChildren(args: GetChildrenArgs): Node[];
    getContent(args: GetContentArgs): Content;
  }

}