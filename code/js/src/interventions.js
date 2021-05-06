"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.sortInterventions = exports.calculateAllInterventionEffects = void 0;
const propagation_1 = require("../src/propagation");
const traversal_1 = require("../src/traversal");
const loops_1 = require("./loops");
const lodash_cloneDeep_1 = __importDefault(require("lodash.cloneDeep"));
const calculateAllInterventionEffects = (graph, deltas, loopRemovalMethod) => {
    const G = lodash_cloneDeep_1.default(graph);
    const defaultDelta = 1;
    const allNodesInG = G.nodes(true);
    const allInterventionEffects = [];
    // Debugging
    if (allNodesInG.length <= 1) {
        console.log('WARNING: Only one node found in network:', allNodesInG);
    }
    // Remove all loops
    if (loopRemovalMethod === undefined || loopRemovalMethod === 'removeAll') {
        for (const node of allNodesInG) {
            const id = node[0]; // class of node = JsnxNode[0]
            loops_1.identifyAndRemoveLoops(G, id);
        }
    }
    for (const node of allNodesInG) {
        const id = node[0]; // class of node = JsnxNode[0]
        // Use intervention delta provided or default
        const d = deltas ? node[1].delta : defaultDelta;
        // Simulate intervention on every node
        const g = lodash_cloneDeep_1.default(G);
        if (loopRemovalMethod === 'removeInterventionWise') {
            loops_1.identifyAndRemoveLoops(g, id);
        }
        const path = traversal_1.calculatePropagationPath(g, id);
        allInterventionEffects.push(propagation_1.propagate(path, id, d));
    }
    return allInterventionEffects;
};
exports.calculateAllInterventionEffects = calculateAllInterventionEffects;
const sortBySumOfEffects = (interventions) => {
    const is = lodash_cloneDeep_1.default(interventions);
    if (is.length > 1) {
        for (const i of is) {
            i.sumOfEffects = Object.values(i.results).reduce((a, b) => a + b);
        }
    }
    else if (is.length === 1) {
        const i = is[0];
        i.sumOfEffects = Object.values(i.results)[0];
    }
    else {
        console.log('WARNING: Some intervention(s) have no effects, not even on themselves');
    }
    is.sort(function (a, b) {
        return b.sumOfEffects - a.sumOfEffects;
    });
    return is;
};
const sortedByEffectOnNode = (interventions, targetNode) => {
    const is = lodash_cloneDeep_1.default(interventions);
    is.sort(function (a, b) {
        return b.results[targetNode] - a.results[targetNode];
    });
    return is;
};
const sortedByBestEffects = (interventions, nodes) => {
    const is = lodash_cloneDeep_1.default(interventions);
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
            const res = i.results;
            for (const [k, v] of Object.entries(res)) {
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