import * as jsnx from 'jsnetworkx';
import { Intervention, JsnxNode, Node } from '../@types/index';
import { propagate } from '../src/propagation';
import { calculatePropagationPath } from '../src/traversal';
import { identifyAndRemoveLoops } from './loops';

export const calculateAllInterventionEffects = (
    G: jsnx.classes.DiGraph,
    deltas?: Node[],
    loopRemovalMethod?: 'all' | 'perIntervention'
): Intervention[] => {
    const defaultDelta = 1;
    const allNodesInG: Node[] = G.nodes(true);

    const allInterventionEffects = [];

    // Remove all loops
    if (!(loopRemovalMethod === 'perIntervention')) {
        for (const node of allNodesInG) {
            const id = node[0];
            identifyAndRemoveLoops(G, id);
        }
    }

    for (const node of allNodesInG) {
        const id = node[0];

        // Use intervention delta provided or default
        const ds: Node[] = deltas ? deltas.filter((x) => x[0] === id) : [];
        const d: number = ds.length === 1 ? ds[0][1].delta : defaultDelta;

        // Simulate intervention on every node
        const g = G;
        if (loopRemovalMethod === 'perIntervention') {
            identifyAndRemoveLoops(g, id);
        }
        const path = calculatePropagationPath(g, id);
        allInterventionEffects.push(propagate(path, id, d));
    }

    return allInterventionEffects;
};

const sortBySumOfEffects = (is: Intervention[]): Intervention[] => {
    for (const i of is) {
        i.sumOfEffects = Object.values(i.results).reduce(
            (a: number, b: number) => a + b
        );
    }
    is.sort(function (a, b) {
        return b.sumOfEffects - a.sumOfEffects;
    });
    return is;
};

const sortedByEffectOnNode = (
    is: Intervention[],
    targetNode: Node['id']
): Intervention[] => {
    is.sort(function (a, b) {
        return b.results[targetNode] - a.results[targetNode];
    });
    return is;
};

const sortedByBestEffects = (
    is: Intervention[],
    nodes: JsnxNode[]
): Intervention[] => {
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
            for (const [k, v] of Object.entries(i.results)) {
                i.sumOfEffects += v * vm(k);
            }
        }
        is.sort(function (a, b) {
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
