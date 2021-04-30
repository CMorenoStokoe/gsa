"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.sortInterventions = exports.calculateAllInterventionEffects = void 0;
const propagation_1 = require("../src/propagation");
const traversal_1 = require("../src/traversal");
const loops_1 = require("./loops");
const calculateAllInterventionEffects = (G, deltas, loopRemovalMethod) => {
    const defaultDelta = 1;
    const allNodesInG = G.nodes(true);
    const allInterventionEffects = [];
    // Remove all loops
    if (!(loopRemovalMethod === 'perIntervention')) {
        for (const node of allNodesInG) {
            const id = node[0];
            loops_1.identifyAndRemoveLoops(G, id);
        }
    }
    for (const node of allNodesInG) {
        const id = node[0];
        // Use intervention delta provided or default
        const ds = deltas ? deltas.filter((x) => x[0] === id) : [];
        const d = ds.length === 1 ? ds[0][1].delta : defaultDelta;
        // Simulate intervention on every node
        const g = G;
        if (loopRemovalMethod === 'perIntervention') {
            loops_1.identifyAndRemoveLoops(g, id);
        }
        const path = traversal_1.calculatePropagationPath(g, id);
        allInterventionEffects.push(propagation_1.propagate(path, id, d));
    }
    return allInterventionEffects;
};
exports.calculateAllInterventionEffects = calculateAllInterventionEffects;
const sortBySumOfEffects = (is) => {
    for (const i of is) {
        i.sumOfEffects = Object.values(i.results).reduce((a, b) => a + b);
    }
    is.sort(function (a, b) {
        return b.sumOfEffects - a.sumOfEffects;
    });
    return is;
};
const sortedByEffectOnNode = (is, targetNode) => {
    is.sort(function (a, b) {
        return b.results[targetNode] - a.results[targetNode];
    });
    return is;
};
const sortedByBestEffects = (is, nodes) => {
    try {
        const vm = (node) => {
            const valence = nodes.filter((x) => x[0] === node)[0][1].valence;
            switch (valence) {
                case 'Bad':
                    return -1;
                case 'Neutral':
                    return 0;
                default:
                    return 1;
            }
        };
        for (const i of is) {
            i.sumOfEffects = 0;
            for (const [k, v] of Object.entries(i.results)) {
                i.sumOfEffects += v * vm(k);
            }
        }
        is.sort(function (a, b) {
            return b.sumOfEffects - a.sumOfEffects;
        });
        return is;
    }
    catch (error) {
        console.error(error);
        return is;
    }
};
const sortInterventions = (is, criteria) => {
    switch (true) {
        case criteria.effectOnNode != undefined:
            return sortedByEffectOnNode(is.slice(), criteria.effectOnNode);
        case criteria.bestEffects != undefined:
            return sortedByBestEffects(is.slice(), criteria.bestEffects);
        default:
            return sortBySumOfEffects(is.slice());
    }
};
exports.sortInterventions = sortInterventions;
//# sourceMappingURL=interventions.js.map