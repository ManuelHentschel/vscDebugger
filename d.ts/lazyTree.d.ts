

export declare module LazyTree {

    // unique to each node. Specifies its position in the NodeList
    // unsigned integer > 0. Use 0 to indicate "NULL-Node" (e.g. parent of root node)
    type NodeId = number; 

    // properties of a (non-lazy) Node. All lazy nodes can be forced to contain these fields
    interface Node {
        nodeId: NodeId;
        parentId: NodeId;
        childrenIds: NodeId[];
        contentContent?: Content;
    }

    // content of a node. Must be extended for each usecase
    interface Content {
        nodeType: string;
    }

    // arguments used to construct a node
    // contentContent is stored to the node itself, children are stored in a new node each
    interface NodeArgs {
        contentContent?: Content, // directly stored to Node.contentContent
        childrenChildren?: NodeArgs[], // used to (recursively) construct child nodes
        preserve?: boolean //=false // keep pre-existing entries? only relevant while appying nodeArgs
    }

    // used to output tree in human-readable form
    interface NestedList extends NodeArgs{
        contentContent: Content;
        childrenChildren: NestedList[];
    }

    class Tree {

        nodes: Node[];

        // // // methods
        // // Construction of the tree

        // returns an instance of Tree
        constructor(
            makeRootNode?: boolean //=false
        )

        // create an empty node with parent specified by parentId
        getNewNodeId(parentId: NodeId): NodeId;

        // create an empty node (is not stored to nodelist yet)
        getEmptyNode(nodeId: NodeId, parentId: NodeId): Node;


        // // Node assignment

        // store node to (already existing) node specified by id
        storeToNode(args: NodeArgs, id: NodeId): void;

        // store node to new node with parent specified by parentId
        storeToNewNode(args: NodeArgs, parentId: NodeId): NodeId;


        // // Tree manipulation

        // remove nodeId from parent nodes' childrenIds and set parentId to 0
        // returns the (former) parentId
        orphanNode(id: NodeId): NodeId;

        // orphan node, then set node and all children to NULL (does not shorten the node list)
        // invalidates the (externally known) nodeIds of all deleted nodes!
        // returns a list of all deleted nodeIds
        deleteNode(id: NodeId): NodeId[];

        // deletes all descendents of a node, returns a list of all deleted nodes
        deleteDescendents(id: NodeId): NodeId[];
        // deletes all descendents of a node, returns a list of only the deleted children
        deleteChildren(id: NodeId): NodeId[];

        // deletes the content of a node, leaves children/parent ids
        // returns the (former) content
        deleteContent(id: NodeId): Content;

        // deletes all consecutive entries that contain only NULL from the end of the node list
        // returns the new last/highest NodeId in the trimmed tree
        trimTree(): NodeId;

        // moves content of a node from id0 to id1 in the nodeList. Updates all parent/child references
        // move only, if the target node is empty
        // returns the new nodeId if successful, the old nodeId otherwise
        moveNode(id0: NodeId, id1: NodeId): NodeId;

        // fills 'holes' (entries that contain NULL) with nodes from the end of the list, then calls trimTree()
        // does not preserve externally known nodeIds!
        shrinkTree(): void;

        // finds the first 'hole' in the nodeList
        // returns the length of the nodeList + 1, if no 'holes' present
        findFirstNull(): NodeId;

        // finds the last entry in the nodeList that is actually in use
        // returns 0L if all nodes are empty
        findLastEntry(): NodeId;


        // // Info retrieval
        // retrieves the content from a node
        getContent(id: NodeId): Content;

        // retrieves the childrenIds of a node
        getChildrenIds(id: NodeId): NodeId[];

        // retrieve parent Id
        getParentId(id: NodeId): NodeId;

        // retrieve ids of ancestors (not including the node itself, including final 0)
        getAncestorIds(id: NodeId): NodeId[];

        // gets the content of a list of nodes
        getContents(ids: NodeId[]): Content[];

        // gets the contents of the children of a node
        getChildrenContents(id: NodeId): Content[];

        // converts the tree to a NestedList  (up to given depth)
        treeToList(id: NodeId, depth: number): NestedList;
    }



    ///////////////////
    // Lazy Stuff


    // used to compute the content of a node
    // result is usually supposed to contain field NodeArgs.contentContent
    type ContentFunction = (args?: ContentArgs) => LazyNodeArgs;

    // used to compute the children of a node
    type ChildrenFunction = (args?: ChildrenArgs) => LazyNodeArgs[];

    // arguments passed to ContentFunction to retrieve content of a node
    interface ContentArgs {
        nodeType: string;
        lazy?: boolean; //=true // Only relevant for LazyTree
        // if true, the result of ContentFuntion also contains children/childrenArgs to construct children
        contentProducesChildren?: boolean; //=false 
    }

    // arguments passed to ChildrenFunction to retrieve list of children
    interface ChildrenArgs {
        nodeType: string;
        lazy?: boolean; //=true // Only relevant for LazyTree
    }

    // lazy extension of Node
    // stores arguments to compute content/children only if requested
    interface LazyNode extends Node {
        childrenReady: boolean;
        childrenArgs?: ChildrenArgs;
        contentReady: boolean;
        contentProducesChildren: boolean;
        contentArgs: ContentArgs;
    }

    // extension of NodeArgs to allow lazy construction of content
    interface LazyNodeArgs extends NodeArgs {
        childrenChildren?: LazyNodeArgs[],
        contentArgs?: ContentArgs, // used to construct content
        childrenArgs?: ChildrenArgs, // used to construct children
        keepArgs?: boolean //=true // keep the args to be able to refresh contents/children?
        // If e.g. both content and contentArgs are specified, the more 'explicit' takes precedence:
        // content == children > contentArgs == childrenArgs > contentProducesChildren
        // if preserve == true: ... > oldEntries (if content/children are not specified, the old entries are preserved)
        // if preserve == false: ... > NULL (if content/children are not specified, the old entries are set to NULL)
    }


    class LazyTree extends Tree {
        // // // properties
        defaultArgs: {
            defaultContentProducesChildren: boolean;
            defaultPreserve: boolean;
            defaultLazy: boolean;
            defaultStoreArgs: boolean;
        }

        childrenFunction: ChildrenFunction;
        contentFunction: ContentFunction;

        nodes: LazyNode[];

        // // // methods
        // // Construction of the tree

        // returns an instance of LazyTree
        constructor(
            childrenFunction?: ChildrenFunction,
            contentFunction?: ContentFunction,
            defaultContentProducesChildren?: boolean, //=false
            defaultPreserve?: boolean, //=false
            defaultLazy?: boolean, //=true
            defaultKeepArgs?: boolean, //=true
            makeRootNode?: boolean //=false
        )


        // // Tree manipulation (lazy stuff)
        // computes the children of a node (if not ready yet)
        forceChildren(id: NodeId, refresh?: boolean): NodeId[];

        // computes the content of a node (if not ready yet)
        forceContent(id: NodeId, refresh?: boolean): Content;

        // recursively calls forceChildren to compute the children of the entire tree (up to given depth)
        forceTree(id: NodeId, depth: number, forceContent: boolean, deleteArgs: boolean, refresh?: boolean): NodeId;

        // recursively deletes all descendents (!) of a node, returns the list of the childrenIds (!)
        // keeps the arguments (if present), i.e. on the next getChildren call, the children are refreshed
        clearChildren(id: NodeId): NodeId[];
        // same as clearChildren but returns a list of all deleted nodes (i.e. also grandchildren etc.)
        clearDescendents(id: NodeId): NodeId[];

        // deletes the content of a node, returns the deleted content
        // keeps the arguments (if present), i.e. on the next getContent call, the content is refreshed
        clearContent(id: NodeId): Content;


        // // Overloaded info retrieval 

        // if refresh==true and contentArgs are present, the content is cleared and computed anew
        // forces and retrieves the content from a node
        getContent(id: NodeId, refresh?: boolean): Content;
        // forces and gets the content of a list of nodes
        getContents(ids: NodeId[], refresh?: boolean): Content[];

        // if refresh==true and childrenArgs are present, the content is cleared and computed anew
        // forces and retrieves the childrenIds of a node
        getChildrenIds(id: NodeId, refresh?: boolean): NodeId[]

        // if refresh==true, the entire children list is refreshed if possible, else only the childrenContents (if possible)
        // forces and gets the contents of the children of a node
        getChildrenContents(id: NodeId, refresh?: boolean): Content[];

        // converts the tree to a NestedList  (up to given depth)
        // refreshes contents/children if requested
        treeToList(id: NodeId, depth: number, refresh?: boolean): NestedList;


        // // Trivial Overloads (Node->LazyNode, NodeArgs->LazyNodeArgs)
        getEmptyNode(nodeId: NodeId, parentId: NodeId): LazyNode;
        storeToNode(args: LazyNodeArgs, id: NodeId): void;
        storeToNewNode(args: LazyNodeArgs, parentId: NodeId): NodeId;
    }
}
