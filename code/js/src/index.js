"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.simulateEverything = exports.simulateIntervention = void 0;
const format_1 = require("./format");
const loops_1 = require("./loops");
const traversal_1 = require("./traversal");
const propagation_1 = require("./propagation");
const interventions_1 = require("./interventions");
const simulateIntervention = (edges, origin, delta, nodes) => {
    // Prepare data
    const G = format_1.formatData(edges, nodes);
    loops_1.identifyAndRemoveLoops(G, origin);
    // Simulate intervention
    const path = traversal_1.calculatePropagationPath(G, origin);
    return propagation_1.propagate(path, origin, delta ? delta : 1);
};
exports.simulateIntervention = simulateIntervention;
const simulateEverything = (edges, nodes, deltasInNodeData, valenceInNodeData, loopRemovalMethod) => {
    // Prepare data
    const G = format_1.formatData(edges, nodes);
    // Simulate all interventions
    const is = interventions_1.calculateAllInterventionEffects(G, deltasInNodeData ? nodes : undefined, loopRemovalMethod);
    /* Sort interventions by effectiveness */
    // By overall effects
    const sortedBySumOfEffects = interventions_1.sortInterventions(is, { sumOfEffects: true });
    // By goodness of effects
    let sortedByBestEffects = [];
    if (valenceInNodeData) {
        sortedByBestEffects = interventions_1.sortInterventions(is, {
            bestEffects: G.nodes(true),
        });
    }
    // By effect on specific node
    const sortedByEffectOnNode = [];
    G.nodes(true).forEach((e) => sortedByEffectOnNode.push({
        node: e[0],
        ranks: interventions_1.sortInterventions(is, { effectOnNode: e[0] }),
    }));
    return {
        G: G,
        unsorted: is,
        sorted: {
            bySumOfEffects: sortedBySumOfEffects,
            byEffectOnNodes: sortedByEffectOnNode,
            byBestEffects: sortedByBestEffects,
        },
    };
};
exports.simulateEverything = simulateEverything;
//# sourceMappingURL=index.js.map