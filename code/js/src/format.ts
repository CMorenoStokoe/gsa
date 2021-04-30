import * as jsnx from 'jsnetworkx';
import { Node, Edge, JsnxNode, JsnxEdge } from '../@types/index';

export const formatData = (
    edges: Edge[],
    nodes?: Node[]
): jsnx.classes.DiGraph => {
    const G: jsnx.classes.DiGraph = new jsnx.DiGraph();
    if (nodes) {
        const arrayOfNodes: JsnxNode[] = nodes.map((x) => [x.id, x]);
        G.addNodesFrom(arrayOfNodes);
    }
    const arrayOfEdges: JsnxEdge[] = edges.map((x) => [x.source, x.target, x]);
    G.addEdgesFrom(arrayOfEdges);
    return G;
};
