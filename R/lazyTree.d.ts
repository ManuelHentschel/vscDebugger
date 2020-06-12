

export declare module LazyTree {

    // unique to each node. Specifies its position in the NodeList
    type NodeId = number;

    // List containing all Nodes, positions corresponding to nodeIds
    type NodeList = LazyNode[];

    // properties of a (non-lazy) Node. All lazy nodes can be forced to contain these fields
    // not actually used in lazy tree
    interface Node {
        nodeId: NodeId;
        parentId: NodeId;
        childrenIds: NodeId[];

        contentContent?: Content;
    }

    // lazy extension of Node
    // stores arguments to compute content/children only if requested
    interface LazyNode extends Node {
        nodeId: NodeId;
        parentId: NodeId;

        childrenIds: NodeId[];
        childrenReady: boolean;
        childrenArgs?: ChildrenArgs;

        contentContent?: Content;
        contentReady: boolean;
        contentProducesChildren: boolean;
        contentArgs: ContentArgs;
    }

    // content of a node. Must be extended for each usecase
    interface Content {
        nodeType: string;
    }

    // arguments passed to ChildrenFunction to retrieve list of children
    interface ChildrenArgs {
        nodeType: string;
        lazy?: boolean; //=true
    }

    // arguments passed to ContentFunction to retrieve content of a node
    interface ContentArgs {
        nodeType: string;
        lazy?: boolean; //=true

        // if true, the result of ContentFuntion also contains children/childrenArgs to construct children
        contentProducesChildren?: boolean; //=false
    }

    // arguments used to construct a node
    interface NodeArgs {
        contentContent?: Content, // directly stored to Node.contentContent
        children?: NodeArgs[], // used to (recursively) construct child nodes

        contentArgs?: ContentArgs, // used to retrieve content when requested
        childrenArgs?: ChildrenArgs, // used to construct children when requested

        preserve?: boolean //=false // only relevant while appying nodeArgs

        // If e.g. both content and contentArgs are specified, the more 'explicit' takes precedence:
        // content == children > contentArgs == childrenArgs > contentProducesChildren
        // if preserve == true: ... > oldEntries (if content/children are not specified, the old entries are preserved)
        // if preserve == false: ... > NULL (if content/children are not specified, the old entries are set to NULL)
    }


    // used to compute the content of a node
    type ContentFunction = (args: ContentArgs) => NodeArgs;

    // used to compute the children of a node
    type ChildrenFunction = (args: ChildrenArgs) => NodeArgs[];

    // used only to output tree in human-readable form
    interface NestedList {
        contentContent: Content;
        children: NestedList[];
    }

    // implemented in R and to be used internally or by the user to modify the tree
    interface globalFunctions {
        // // Construction of the tree

        // delete existing tree and make a new tree with an empty root node
        makeNewtree(contentFunction: ContentFunction, childrenFunction: ChildrenFunction): void;

        // create an empty node with parent specified by parentId
        getNewNodeId(parentId: NodeId): NodeId;

        // create an empty node (is not stored to nodelist yet)
        getEmptyNode(nodeId: NodeId, parentId: NodeId): LazyNode;


        // // Node assignment

        // store node to (already existing) node specified by id
        storeToNode(args: NodeArgs, id: NodeId): void;

        // store node to new node with parent specified by parentId
        storeToNewNode(args: NodeArgs, parentId: NodeId): NodeId;


        // // Tree manipulation (non-lazy stuff)

        // remove nodeId from parent nodes childrenIds and set parentId to 0
        orphanNode(id: NodeId): NodeId;

        // orphan node, then set node and all children to NULL (does not shorten the node list)
        // invalidates the nodeIds of all deleted nodes!
        deleteNode(id: NodeId): NodeId;

        // deletes all consecutive entries that contain only NULL from the end of the node list
        trimTree(): void;

        // moves content of a node from id0 to id1 in the nodeList. Updates all parent/child references
        // does not preserve externally known nodeIds!
        moveNode(id0: NodeId, id1: NodeId): NodeId;

        // fills 'holes' (entries that contain NULL) with nodes from the end of the list, then calls trimTree()
        // does not preserve externally known nodeIds!
        shrinkTree(): void;

        // finds the first 'hole' in the nodeList
        findFirstNull(): NodeId;

        // finds the last entry in the nodeList that is actually in use
        findLastEntry(): NodeId;


        // // Tree manipulation (lazy stuff)

        // computes the children of a node (if not ready yet)
        forceChildren(id: NodeId): NodeId[];

        // computesthe content of a node (if not ready yet)
        forceContent(id: NodeId): Content;

        // recursively calls forceChildren to compute teh children of the entire tree (up to given depth)
        forceTree(id: NodeId, depth: number, forceContent: boolean, deleteArgs: boolean): NodeId;


        // // Info retrieval (work 'lazy-agnostic', everything is forced before returning)

        // forces and retrieves the content from a node
        getContent(id: NodeId): Content;

        // forces and retrieves the childrenIds of a node
        getChildrenIds(id: NodeId): NodeId[];

        // forces and gets the content of a list of nodes
        getContents(ids: NodeId[]): Content[];

        // forces and gets the contents of the children of a node
        getChildrenContents(id: NodeId): Content[];

        // forces and converts the tree to a NestedList  (up to given depth)
        treeToList(id: NodeId, depth: number): NestedList;
    }
}