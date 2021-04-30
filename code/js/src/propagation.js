"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.propagate = void 0;
const propagate = (path, origin, valueChange) => {
    const individualSteps = [];
    const results = {};
    results[origin] = valueChange; // Effect of intervention on origin
    for (const edge of path) {
        // Two-step MR
        const beta = edge.beta;
        const deltaX = results[edge.source];
        const deltaY = deltaX * beta;
        // Save individual step to history
        individualSteps.push({
            source: edge.source,
            deltaX: deltaX,
            target: edge.target,
            beta: beta,
            deltaY: deltaY,
        });
        // Update total changes
        if (results[edge.target]) {
            results[edge.target] += deltaY;
        }
        else {
            results[edge.target] = deltaY;
        }
    }
    return {
        origin: origin,
        steps: individualSteps,
        results: results,
    };
};
exports.propagate = propagate;
//# sourceMappingURL=propagation.js.map