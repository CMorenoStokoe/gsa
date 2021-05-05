import * as jsnx from 'jsnetworkx';
import { Intervention, JsnxNode, Node } from '../@types/index';
import { propagate } from '../src/propagation';
import { calculatePropagationPath } from '../src/traversal';
import { identifyAndRemoveLoops } from './loops';
import cloneDeep from 'lodash.cloneDeep';

export const calculateAllInterventionEffects = (
    graph: jsnx.classes.DiGraph,
    deltas?: Node[],
    loopRemovalMethod?: 'removeAll' | 'removeInterventionWise' | 'noLoopRemoval'
): Intervention[] => {
    const G = cloneDeep(graph);

    const defaultDelta = 1;
    const allNodesInG: JsnxNode[] = G.nodes(true);

    const allInterventionEffects = [];

    // Debugging
    if (allNodesInG.length <= 1) {
        console.log('WARNING: Only one node found in network:', allNodesInG);
    }

    // Remove all loops
    if (loopRemovalMethod === undefined || loopRemovalMethod === 'removeAll') {
        for (const node of allNodesInG) {
            const id = node[0]; // class of node = JsnxNode[0]
            identifyAndRemoveLoops(G, id);
        }
    }

    for (const node of allNodesInG) {
        const id: string = node[0]; // class of node = JsnxNode[0]

        // Use intervention delta provided or default
        const ds: Node[] = deltas ? deltas.filter((x) => x[0] === id) : [];
        const d: number = ds.length === 1 ? ds[0][1].delta : defaultDelta;

        // Simulate intervention on every node
        const g = cloneDeep(G);
        if (loopRemovalMethod === 'removeInterventionWise') {
            identifyAndRemoveLoops(g, id);
        }
        const path = calculatePropagationPath(g, id);
        allInterventionEffects.push(propagate(path, id, d));
    }

    return allInterventionEffects;
};

const sortBySumOfEffects = (interventions: Intervention[]): Intervention[] => {
    const is = cloneDeep(interventions);
    if (is.length > 1) {
        for (const i of is) {
            i.sumOfEffects = Object.values(i.results).reduce(
                (a: number, b: number) => a + b
            );
        }
    } else if (is.length === 1) {
        const i = is[0];
        i.sumOfEffects = Object.values(i.results)[0];
    } else {
        console.log(
            'WARNING: Some intervention(s) have no effects, not even on themselves'
        );
    }
    is.sort(function (a, b) {
        return b.sumOfEffects - a.sumOfEffects;
    });
    return is;
};

const sortedByEffectOnNode = (
    interventions: Intervention[],
    targetNode: Node['id']
): Intervention[] => {
    const is = cloneDeep(interventions);
    is.sort(function (a: Intervention, b: Intervention) {
        return b.results[targetNode] - a.results[targetNode];
    });
    return is;
};

const sortedByBestEffects = (
    interventions: Intervention[],
    nodes: JsnxNode[]
): Intervention[] => {
    const is = cloneDeep(interventions);
    try {
        const vm = (node: Node['id']) => {
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
            const res: Intervention['results'] = i.results;
            for (const [k, v] of Object.entries(res)) {
                i.sumOfEffects += v * vm(k);
            }
        }
        is.sort(function (a: Intervention, b: Intervention) {
            return b.sumOfEffects - a.sumOfEffects;
        });
        return is;
    } catch (error) {
        console.error(error);
        return is;
    }
};

export const sortInterventions = (
    is: Intervention[],
    criteria: {
        sumOfEffects?: true;
        effectOnNode?: Node['id'];
        bestEffects?: JsnxNode[];
    }
): Intervention[] => {
    switch (true) {
        case criteria.effectOnNode != undefined:
            return sortedByEffectOnNode(is.slice(), criteria.effectOnNode);
        case criteria.bestEffects != undefined:
            return sortedByBestEffects(is.slice(), criteria.bestEffects);
        default:
            return sortBySumOfEffects(is.slice());
    }
};
