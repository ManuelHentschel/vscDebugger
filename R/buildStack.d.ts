
// buildStack() gathers information about current stackframes/scopes
// structured as follows (using nested named lists):

import {DebugProtocol} from './DebugProtocol';

import { LazyTree } from './lazyTree';


export declare module StackTree {

    interface Stack extends LazyTree.Content {
        totalFrames?: number,
    }

    interface StackFrame extends DebugProtocol.StackFrame, LazyTree.Content {
        // set name = '' + id
        variablesReference: number;
        frameIdR: number;
    }

    interface Scope extends DebugProtocol.Scope, LazyTree.Content {
        name: string;
        rValue: REnvironment;
        variablesReference: number;
        setInfo?: SetInfo; // sensible here?
    }

    interface SetInfo {
        setter?: RValue, // e.g. quote(v[1,]), sufficient for non-root elements

        expression?: RValue; //only required for root element
        environment?: REnvironment; // only required for root element
    }

    interface Variable extends MinimalVariable, DebugProtocol.Variable, LazyTree.Content {
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

    interface MinimalVariable {
        name: string;
        rValue: RValue;

        setter?: RValue;
        setInfo?: SetInfo;
    }

    interface Source extends DebugProtocol.Source {
        name: string;
        path: string;
        sourceReference: number;
        line: number;
        endLine: number;
        column: number;
        endColumn: number;
        srcbody: string;
        isFile: boolean;
        presentationHint?: 'normal' | 'emphasize' | 'deemphasize';
    }



    type REnvironment = RValue;

    type RValue = any;
    type RCall = any;
    type NULL = undefined;
    type ChildVarFunction = (rValue: RValue) => MinimalVariable[];

    interface varInfo {
        name: string;
        doesApply: ((v: RValue) => boolean);
        childVars?: ChildVarFunction | MinimalVariable[];
        customAttributes?: ChildVarFunction | MinimalVariable[];
        internalAttributes?: ChildVarFunction | MinimalVariable[];
        hasChildren?: ((v:RValue) => boolean) | boolean;
        toString?: ((v:RValue) => string) | string;
        type?: ((v:RValue) => string) | string;
        includeAttributes?: ((v:RValue) => boolean) | boolean;
        evaluateName?: ((v:RValue) => string) | string;
    }

    type varInfos = varInfo[];

    interface LineAndFile extends DebugProtocol.Source {
        // name?: string // in DebugProtocol.Source
        // path?: string // in DebugProtocol.Source
        // sourceReference?: number // in DebugProtocol.Source
        line?: number,
        endLine?: number,
        column?: number,
        endColumn?: number
        srcbody?: string,
        isFile?: boolean
    }

    function getSource(call?: RValue, frameIdR?: number): Source;


    function _vsc_buildStack(): LazyTree.NodeId;




    interface VscContent extends LazyTree.Content {
        nodeType: ('Stack'|'Frame'|'Scope'|'Variable');
    }

    interface VscChildrenArgs extends LazyTree.ChildrenArgs {
        nodeType: ('Frame'|'Scope'|'Variable');
    }

    interface VscContentArgs extends LazyTree.ContentArgs {
        nodeType: ('Stack'|'Frame'|'Scope'|'Variable');
    }


    function buildStack(args: stackArgs): {
        contentContent: Stack;
        childrenArgs: FramesArgs;
    };
    function gatherFrames(args: FramesArgs): {
        contentArgs: FrameArgs;
        // childrenArgs: ScopesArgs;
    }[]

    function buildFrame(args: FrameArgs): {
        contentContent: StackFrame;
        childrenArgs: ScopesArgs;
    }

    function gatherScopes(args: ScopesArgs): {
        contentArgs: ScopeArgs;
        // childrenArgs: VariablesArgs;
    }[]

    function buildVariable(args: VariableArgs): {
        contentContent: Variable;
        childrenArgs: VariablesArgs;
    };
    function gatherVariables(args: VariablesArgs): {
        contentArgs: VariableArgs;
        // childrenArgs: VariablesArgs;
    }[]


    interface stackArgs extends LazyTree.ContentArgs, FramesArgs {
    }

    interface FramesArgs extends LazyTree.ChildrenArgs {
        topFrame: REnvironment;
        skipFromTop?: number;
        skipFromBottom?: number;
        isError: boolean;
        forceDummyStack?: boolean;
        dummyFile?: string;
    }

    interface FrameArgs extends LazyTree.ContentArgs {
        frameIdR: number;
        frameIdVsc: number;
        dummyFile?: string;
    }

    interface ScopesArgs extends LazyTree.ChildrenArgs {
        firstenv: REnvironment;
        lastenv: REnvironment;
    }

    interface VariableArgs extends LazyTree.ContentArgs {
        minVar: MinimalVariable;
    }

    interface ScopeArgs extends VariableArgs {
    }

    interface VariablesArgs extends LazyTree.ChildrenArgs {
        rValue: RValue;
    }

}