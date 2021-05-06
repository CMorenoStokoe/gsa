import * as jsnx from 'jsnetworkx';
import { Node, Edge, Intervention, JsnxNode } from '../@types/index';
import { formatData } from './format';
import { identifyAndRemoveLoops } from './loops';
import { calculatePropagationPath } from './traversal';
import { propagate } from './propagation';
import {
    calculateAllInterventionEffects,
    sortInterventions,
} from './interventions';

export const simulateIntervention = (
    edges: Edge[],
    origin: Node['id'],
    delta?: number,
    nodes?: Node[]
): Intervention => {
    // Prepare data
    const G: jsnx.classes.DiGraph = formatData(edges, nodes);
    identifyAndRemoveLoops(G, origin);
    // Simulate intervention
    const path = calculatePropagationPath(G, origin);
    return propagate(path, origin, delta ? delta : 1);
};

export const simulateEverything = (
    edges: Edge[],
    nodes?: Node[],
    deltasInNodeData?: boolean,
    valenceInNodeData?: boolean,
    loopRemovalMethod?: 'removeAll' | 'removeInterventionWise' | 'noLoopRemoval'
): {
    G: jsnx.classes.DiGraph;
    unsorted: Intervention[];
    sorted: {
        bySumOfEffects: Intervention[];
        byEffectOnNodes: {
            node: Node['id'];
            ranks: Intervention[];
        }[];
        byBestEffects: Intervention[];
    };
} => {
    // Prepare data
    const G: jsnx.classes.DiGraph = formatData(edges, nodes);

    // Simulate all interventions
    const is = calculateAllInterventionEffects(
        G,
        deltasInNodeData ? true : undefined,
        loopRemovalMethod
    );

    /* Sort interventions by effectiveness */
    // By overall effects
    const sortedBySumOfEffects = sortInterventions(is, { sumOfEffects: true });

    // By goodness of effects
    let sortedByBestEffects = [];
    if (valenceInNodeData) {
        sortedByBestEffects = sortInterventions(is, {
            bestEffects: G.nodes(true),
        });
    }

    // By effect on specific node
    const sortedByEffectOnNode: {
        node: Node['id'];
        ranks: Intervention[];
    }[] = [];
    G.nodes(true).forEach((e: JsnxNode) =>
        sortedByEffectOnNode.push({
            node: e[0],
            ranks: sortInterventions(is, { effectOnNode: e[0] }),
        })
    );

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
