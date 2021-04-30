import { simulateEverything } from './index';
import { Node, Edge } from '../@types/index';
import estimates from './trait estimates.json';
import { nodeValues, isGood } from './trait extra info';
//import * as fs from 'fs';

const seed: Edge[] = [
    { source: 'Exercise', target: 'Coffee intake' },
    { source: 'Intelligence', target: 'Exercise' },
    { source: 'Intelligence', target: 'Coffee intake' },
    { source: 'Intelligence', target: 'CHD' },
    { source: 'Intelligence', target: 'Not socialising' },
    { source: 'Alcohol', target: 'Eveningness' },
    { source: 'Alcohol', target: 'Education' },
    { source: 'Alcohol', target: 'Not socialising' },
    { source: 'Alcohol', target: 'Intelligence' },
    { source: 'Alcohol', target: 'BMI' },
    { source: 'Education', target: 'Neuroticism' },
    { source: 'Education', target: 'Eveningness' },
    { source: 'Education', target: 'Smoking' },
    { source: 'Education', target: 'BMI' },
    { source: 'Education', target: 'CHD' },
    { source: 'Education', target: 'Intelligence' },
    { source: 'Education', target: 'Not socialising' },
    { source: 'Education', target: 'Coffee intake' },
    { source: 'Education', target: 'Exercise' },
    { source: 'Sleeplessness', target: 'Alcohol' },
    { source: 'Sleeplessness', target: 'Education' },
    { source: 'Sleeplessness', target: 'BMI' },
    { source: 'Sleeplessness', target: 'CHD' },
    { source: 'Sleeplessness', target: 'Not socialising' },
    { source: 'Sleeplessness', target: 'Wellbeing' },
    { source: 'BMI', target: 'Intelligence' },
    { source: 'BMI', target: 'Coffee intake' },
    { source: 'BMI', target: 'Smoking' },
    { source: 'BMI', target: 'CHD' },
    { source: 'BMI', target: 'Diabetes' },
    { source: 'BMI', target: 'Not socialising' },
    { source: 'Loneliness', target: 'Neuroticism' },
    { source: 'Loneliness', target: 'Sleeplessness' },
    { source: 'Worry', target: 'Loneliness' },
    { source: 'Worry', target: 'Wellbeing' },
    { source: 'Worry', target: 'Sleeplessness' },
    { source: 'Worry', target: 'Neuroticism' },
    { source: 'Depression', target: 'Worry' },
    { source: 'Depression', target: 'Neuroticism' },
    { source: 'Depression', target: 'Wellbeing' },
    { source: 'Depression', target: 'Loneliness' },
    { source: 'Depression', target: 'Sleeplessness' },
]; // Get edges in game (loop detection + p-value seed; deletes edges differently if ran in a different order, as well as different p value thresholds removing edges by threshold)

const isInGame = (s: string, t?: string): boolean => {
    if (t) {
        // Check nodes
        for (const e of seed) {
            if (e.source == s && e.target == t) {
                return true;
            }
        }
    } else {
        // Check edges
        for (const e of seed) {
            if (e.source == s || e.target == s) {
                return true;
            }
        }
    }
    return false;
};
const edges = (): Edge[] => {
    const output: Edge[] = [];
    estimates.links.forEach((e) => {
        if (isInGame(e['exposure'], e['outcome'])) {
            output.push({
                source: e['exposure'],
                target: e['outcome'],
                beta: Number(e['b']),
            });
        }
    });
    return output;
};
const nodes = (): Node[] => {
    const output: Node[] = [];
    estimates.nodes.forEach((e) => {
        if (isInGame(e['label'])) {
            // Exclude happiness since it wasn't in game / test
            output.push({
                id: e['label'],
                delta:
                    nodeValues[e['id']].units === 'Odds (%)'
                        ? nodeValues[e['id']].prevalence * 0.33
                        : 1,
                valence: isGood[e['id']],
            });
        }
    });
    return output;
};

/* 

TO DO

Sort out bey best effects */
const e = edges();
const n = nodes();
const results = simulateEverything(edges(), nodes(), true, true);
// FIX: BEST INTERVENTIONS TAKE NODE LIST NOT JSNX NODELIST
const x = nodes().filter((x) => x.id === 'Depression');
const tests = {
    sameNNodes: nodes().length === 17,
    sameNEdges: edges().length === 42,
    correctTotalPossibleInterventions: results.unsorted.length === 17,
    highestRankingIsDirect:
        results.sorted.byEffectOnNodes[0].ranks[0].origin === 'Depression',
    lowerRankingsAreLower:
        results.sorted.byEffectOnNodes[6].ranks[1].results.Alcohol >=
            results.sorted.byEffectOnNodes[6].ranks[2].results.Alcohol &&
        results.sorted.byEffectOnNodes[6].ranks[3].results.Alcohol >=
            results.sorted.byEffectOnNodes[6].ranks[4].results.Alcohol,
};

//fs.writeFile('data.json', results);

debugger;
