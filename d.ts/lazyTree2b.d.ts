


import { RValue, RFunction, RCall, RNULL, RList, RVector } from './RTypes';
import { DebugProtocol } from './DebugProtocol';

export declare module StackTree {

	class REnvironment {}

	interface SetInfo {
		setter?: RValue, // e.g. quote(v[1,]), sufficient for non-root elements
		expression?: RValue; //only required for root element
		environment?: REnvironment; // only required for root element
	}
	

	// used to output tree in human-readable form
	interface NestedList {
		contentContent: Content;
		childrenChildren: NestedList[];
	}


	class Tree extends REnvironment {
		rootNode?: Node;
	}

	interface GetChildrenArgs {
		// interfaceType: "GetChildrenArgs";
		refresh?: boolean;
		nodeType: NodeType;
	}
	interface GetVariablesArgs extends GetChildrenArgs {
		nodeType: NodeType.Variable;
		filter?: 'indexed'|'named';
		start?: number;
		count?: number;
		items?: string[];
		getFromArguments?: boolean; // = false
	}
	interface GetScopesArgs extends GetChildrenArgs {
		nodeType: NodeType.Scope;
	}
	interface GetFramesArgs extends GetChildrenArgs {
		nodeType: NodeType.Frame;
		startFrame?: number;
		levels?: number;
	}
	type GenericGetChildrenArgs<T extends NodeType> = (
		T extends NodeType.Stack ? GetFramesArgs :
		T extends NodeType.Frame ? GetScopesArgs :
		T extends NodeType.Scope ? GetVariablesArgs :
		T extends NodeType.Variable ? GetVariablesArgs :
		GetChildrenArgs
	)


	interface GetContentArgs {
		// interfaceType: "GetContentArgs";
		nodeType: NodeType;
		refresh?: boolean;
	}
	interface GetVariableArgs extends GetContentArgs {
		nodeType: NodeType.Variable;
	}
	interface GetScopeArgs extends GetContentArgs {
		nodeType: NodeType.Scope;
	}
	interface GetFrameArgs extends GetContentArgs {
		nodeType: NodeType.Frame;
	}
	interface GetStackArgs extends GetContentArgs {
		nodeType: NodeType.Stack;
	}
	type GenericGetContentArgs<T extends NodeType> = (
		T extends NodeType.Stack ? GetStackArgs :
		T extends NodeType.Scope ? GetScopeArgs :
		T extends NodeType.Frame ? GetFrameArgs :
		T extends NodeType.Variable ? GetVariableArgs :
		GetContentArgs
	)

	interface Content {
		// interfaceType: "Content";
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
		
		hasChildren: boolean;
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

	interface MakeContentArgs {
		// interfaceType: "MakeContentArgs";
		nodeType: NodeType;
	}
	interface MakeMetaArgs extends MakeContentArgs {
		nodeType: NodeType.Meta;
	}
	interface MakeVariableArgs extends MakeContentArgs {
		nodeType: NodeType.Variable;
		name: string;
		rValue: RValue;
		setter?: RValue;
		setInfo?: SetInfo;
		filter?: 'indexed'|'named';
		start?: number;
		count?: number;
		items?: string[];
	}
	interface MakeScopeArgs extends MakeContentArgs {
		nodeType: NodeType.Scope;
		name: string;
		rValue: RValue;
		setter?: RValue;
		setInfo?: SetInfo;
	}
	interface MakeFrameArgs extends MakeContentArgs {
		nodeType: NodeType.Frame;
		frameIdR: number;
		frameIdVsc: number;
		dummyFile?: string; // determine in R?
		isDummyFrame?: boolean; // determine in R?
	}
	interface MakeStackArgs extends MakeContentArgs {
		nodeType: NodeType.Stack;
	}
	type GenericMakeContentArgs<T extends NodeType> = (
		T extends NodeType.Stack ? MakeStackArgs :
		T extends NodeType.Scope ? MakeScopeArgs :
		T extends NodeType.Frame ? MakeFrameArgs :
		T extends NodeType.Variable ? MakeVariableArgs :
		MakeContentArgs
	)


	interface MakeChildrenArgs {
		// interfaceType: "ChildrenArgs";
		parent: Node;
	}
	interface MakeFramesArgs extends MakeChildrenArgs {
		nodeType: NodeType.Frame;
		topFrameId: number;
		skipFromTop?: number;
		skipFromBottom?: number;
		isError: boolean;
		forceDummystack?: boolean;
		dummyFile?: string;
		startFrame?: number; // from DebugProtocol.StackTraceRequest
		levels?: number; // from DebugProtocol.StackTraceRequest
	}
	interface MakeScopesArgs extends MakeChildrenArgs {
		nodeType: NodeType.Scope;
		firstenv: REnvironment;
		lastenv: REnvironment;
	}
	interface MakeVariablesArgs extends MakeChildrenArgs {
		nodeType: NodeType.Variable;
		rValue: RValue;
	}
	type GenericMakeChildrenArgs<T extends NodeType> = (
		T extends NodeType.Stack ? MakeFramesArgs :
		T extends NodeType.Frame ? MakeScopesArgs :
		T extends NodeType.Scope ? MakeVariablesArgs :
		T extends NodeType.Variable ? MakeVariablesArgs :
		MakeChildrenArgs
	)

	type NodeId = number;
	// type NodeType = "Meta"|"Stack"|"Frame"|"Scope"|"Variable";
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

	type MakeFrames = (args: MakeFramesArgs) => FrameNode[];
	type MakeScopes = (args: MakeScopesArgs) => ScopeNode[];
	type MakeVariables = (args: MakeVariablesArgs) => VariableNode[];

	type MakeStack = (args: MakeStackArgs) => Stack;
	type MakeFrame = (args: MakeFrameArgs) => Frame;
	type MakeScope = (args: MakeScopeArgs) => Scope;
	type MakeVariable = (args: MakeVariableArgs) => Variable;


	// type childNodeType<"Stack"> = "Scope";

	class Node2 extends REnvironment {
		nodeType: NodeType;
		childNodeType: NodeType;
		parent: Node;

		nodeArgs: object;
		
	}

	class Node extends REnvironment {
		
		nodeId: NodeId;
		nodeType: NodeType;
		childNodeType: NodeType;
		childIndex?: number; // as used by parent node

		static makeRootNode(): Node;

		parent: Node;
		getParent(): Node[];
		getAncestors(includeThisNode?: boolean): Node[];

		asList(): NestedList;

		makeChildren(args: MakeChildrenArgs): Node[];
		getChildren(args: GetChildrenArgs): Node[];
		childrenArgs?: MakeChildrenArgs
		children?: Node[];
		childIndices?: number[];

		makeContent(args: MakeContentArgs): Content;
		getContent(args: GetContentArgs): Content;
		content?: Content
		contentArgs?: MakeContentArgs
	}


	class GenericNode<T extends NodeType> extends Node {
		nodeType: T;
		nodeDescription: string;
		childNodeType: ChildNodeType<T>;
		parent: GenericNode<ParentNodeType<T>>;

		makeChildren(args: GenericMakeChildrenArgs<T>): GenericNode<ChildNodeType<T>>[];
		getChildren(args: GenericGetChildrenArgs<T>): GenericNode<ChildNodeType<T>>[];
		children?: GenericNode<ChildNodeType<T>>[];
		childrenArgs: GenericMakeChildrenArgs<T>;

		makeContent(args: GenericMakeContentArgs<T>): GenericContent<T>;
		getContent(args: GenericGetContentArgs<T>): GenericContent<T>;
		content?: GenericContent<T>;
		contentArgs?: GenericMakeContentArgs<T>;
	}


	class StackNode extends GenericNode<NodeType.Stack> {
		// makeChildren(args: MakeFramesArgs): FrameNode[];
		makeChildren: MakeFrames;
		getChildren(args: GetFramesArgs): FrameNode[];
		children?: FrameNode[];
		childrenArgs: MakeFramesArgs;

		makeContent: MakeStack;
		getContent(args: GetStackArgs): Stack;
		content?: Stack;
		contentArgs: MakeStackArgs;
	}

	class FrameNode extends GenericNode<NodeType.Frame> {
		makeChildren: MakeScopes;
		getChildren(args: GetScopesArgs): ScopeNode[];
		children?: ScopeNode[];
		childrenArgs: MakeScopesArgs;

		makeContent: MakeFrame;
		getContent(args: GetFrameArgs): Frame;
		content?: Frame;
		contentArgs: MakeFrameArgs;
	}

	class ScopeNode extends GenericNode<NodeType.Scope> {
		makeChildren: MakeVariables;
		getChildren(args: GetVariablesArgs): VariableNode[];
		children?: VariableNode[];
		childrenArgs: MakeVariablesArgs;

		makeContent: MakeScope;
		getContent(args: GetScopeArgs): Scope;
		content?: Scope;
		contentArgs: MakeScopeArgs;
	}

	class VariableNode extends GenericNode<NodeType.Variable> {
		makeChildren: MakeVariables;
		getChildren(args: GetVariablesArgs): VariableNode[];
		children?: VariableNode[];
		childrenArgs: MakeVariablesArgs;

		makeContent: MakeVariable;
		getContent(args: GetVariableArgs): Variable;
		content?: Variable;
		contentArgs: MakeVariableArgs;
	}
}

