"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.calculatePropagationPath = void 0;
// Run DFS
const calculatePropagationPath = (G, root) => {
    // Init search queue
    const queue = [root];
    // Path of traversal for propagation
    const path = [];
    // Run search with failsafe to avoid infinite loops (max iterations = n nodes ^ 2)
    for (let i = 0; i < Math.pow(G.nodes().length, 2); i++) {
        if (queue[0] == undefined) {
            break;
        }
        // Construct traversal path from successor nodes
        const successors = G.successors(queue[0]);
        for (const successor of successors) {
            // Get edge beta
            const beta = G.getEdgeData(queue[0], successor).beta;
            // Add edge to search path
            // Add node to search path (if not already queued)
            if (path.filter((item) => item.source == queue[0] && item.target == successor).length == 0) {
                path.push({ source: queue[0], target: successor, beta: beta });
            }
            // Add to queue
            queue.push(successor);
        }
        // Remove current item from queue
        queue.shift();
    }
    return path;
};
exports.calculatePropagationPath = calculatePropagationPath;
//# sourceMappingURL=traversal.js.map