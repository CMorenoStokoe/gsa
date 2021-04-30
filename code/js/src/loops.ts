import * as jsnx from 'jsnetworkx';
import { Node, JsnxEdge } from '../@types/index';

// Find loops from graph
export const identifyAndRemoveLoops = (
    G: jsnx.classes.DiGraph,
    root: Node['id']
): JsnxEdge[] => {
    const queue: Node['id'][] = [root];
    const loops: JsnxEdge[] = [];

    // Use colors to keep track of visited nodes (white = not visited, grey = visiting, black = visited)
    const color: Record<Node['id'], string> = {};

    // Run traversal
    search(queue[0]);

    // Get successors of node
    function search(node: Node['id']) {
        // Color node as visiting
        color[node] = 'grey';

        const successors: Node['id'][] = G.successors(node);
        for (const successor of successors) {
            if (color[successor] === 'grey') {
                // If a loop is found, remove it from the Graph and continue
                G.removeEdge(node, successor);
                loops.push([node, successor]);
            } else {
                search(successor);
            }
        }

        // Once done, color node as visited
        color[node] = 'black';
    }

    return loops;
};
