"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const format_1 = require("./format");
const trait_estimates_json_1 = __importDefault(require("./trait estimates.json"));
const trait_extra_info_1 = require("./trait extra info");
const objects_to_csv_1 = __importDefault(require("objects-to-csv"));
const jsnx = __importStar(require("jsnetworkx"));
// Prepare data
const seed = [
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
const isInGame = (s, t) => {
    if (t) {
        // Check nodes
        for (const e of seed) {
            if (e.source == s && e.target == t) {
                return true;
            }
        }
    }
    else {
        // Check edges
        for (const e of seed) {
            if (e.source == s || e.target == s) {
                return true;
            }
        }
    }
    return false;
};
const edges = () => {
    const output = [];
    trait_estimates_json_1.default.links.forEach((e) => {
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
const nodes = () => {
    const output = [];
    const calcDelta = (id, units, prevalenceForOddsCalc, valence) => {
        let d = 1;
        /* NOTE: In game, odds interventions did changed by a different amount to SD, this was in attempt to standardise the visual effects */
        //let d = units === 'Odds (%)' ? prevalenceForOddsCalc * 0.33 : 1;
        if (id === 'BMI') {
            d = -d;
        }
        else if (id === 'Coffee consumption' || id === 'Eveningness') {
            d = d;
        }
        else {
            if (valence === 'Bad') {
                d *= -1;
            }
        }
        return d;
    };
    trait_estimates_json_1.default.nodes.forEach((e) => {
        if (isInGame(e['label'])) {
            // Exclude happiness since it wasn't in game / test
            output.push({
                id: e['label'],
                delta: calcDelta(e['label'], trait_extra_info_1.nodeValues[e['id']].units, trait_extra_info_1.nodeValues[e['id']].prevalence, trait_extra_info_1.isGood[e['id']]),
                valence: trait_extra_info_1.isGood[e['id']],
            });
        }
    });
    return output;
};
/*
// Run analysis
const results = simulateEverything(
    edges(),
    nodes(),
    true,
    true,
    'noLoopRemoval'
);

// Test results
const t = (
    id: string,
    criteria: Array<'>' | '<' | '=' | number>[]
): boolean => {
    const sum = results.sorted.bySumOfEffects.filter((x) => x.origin === id)[0]
        .sumOfEffects;
    const goodness = results.sorted.byBestEffects.filter(
        (x) => x.origin === id
    )[0].sumOfEffects;
    let effectDepression = results.sorted.byEffectOnNodes
        .filter((x) => x.node === 'Depression')[0]
        .ranks.filter((x) => x.origin === id)[0].results.Depression;
    if (effectDepression === undefined) {
        effectDepression = 0;
    }
    let effectEveningness = results.sorted.byEffectOnNodes
        .filter((x) => x.node === 'Eveningness')[0]
        .ranks.filter((x) => x.origin === id)[0].results.Eveningness;
    if (effectEveningness === undefined) {
        effectEveningness = 0;
    }
    const answers: Array<number> = [
        sum,
        goodness,
        effectDepression,
        effectEveningness,
    ];
    for (const i in answers) {
        let o = false;
        const q = criteria[i][0];
        const a = criteria[i][1];
        let t = null;
        switch (q) {
            case '>':
                t = '>';
                o = answers[i] > a;
                break;
            case '<':
                t = '<';
                o = answers[i] < a;
                break;
            default:
                t = '===';
                o = answers[i] === a;
                break;
        }
        if (o === false) {
            console.log(
                'ERROR: Failed test',
                `${id}, #${i} (${answers[i]} ! ${q} (${t}) ${a})`
            );
            return false;
        }
    }
    return true;
};
const tests: Record<string, boolean> = {
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
    sensibleResult_eveningness: t('Eveningness', [
        ['=', 1],
        ['=', 0],
        ['=', 0],
        ['=', 1],
    ]),
    sensibleResult_depression: t('Depression', [
        ['<', 0],
        ['>', 0],
        ['=', -1],
        ['>', 0],
    ]),
    sensibleResult_exercise: t('Exercise', [
        ['<', 1],
        ['=', 1],
        ['=', 0],
        ['=', 0],
    ]),
    sensibleResult_education: t('Education', [
        ['<', 0],
        ['>', 1],
        ['=', 0],
        ['>', 0],
    ]),
};
tests;

//  Format outputs for saving
const formatDataForCSV = (
    interventions: Intervention[]
): InterventionSummary[] => {
    const formattedData: InterventionSummary[] = interventions.map((x) => {
        return {
            ORIGIN: x.origin,
            SCORE: x.sumOfEffects,
            STEPS: x.steps.length,
            Depression: x.results.Depression,
            Worry: x.results.Worry,
            Wellbeing: x.results.Wellbeing,
            Loneliness: x.results.Loneliness,
            Sleeplessness: x.results.Sleeplessness,
            Neuroticism: x.results.Neuroticism,
            Alcohol: x.results.Alcohol,
            Education: x.results.Education,
            BMI: x.results.BMI,
            Intelligence: x.results.Intelligence,
            Eveningness: x.results.Eveningness,
            'Not socialising': x.results['Not socialising'],
            Smoking: x.results.Smoking,
            Exercise: x.results.Exercise,
            'Coffee intake': x.results['Coffee intake'],
            CHD: x.results.CHD,
            Diabetes: x.results.Diabetes,
        };
    });
    return formattedData;
};
const output = formatDataForCSV(results.sorted.byBestEffects);

// Save results
(async () => {
    const csv = new ObjectsToCsv(output);

    // Save to file:
    await csv.toDisk('allInterventionScores.csv');

    // Return the CSV file as string:
    console.log(await csv.toString());
})();

*/
// Perform network property analysis
const G = format_1.formatData(edges(), nodes());
const properties = {};
// Neighbourhood
for (const node of G.nodes()) {
    properties[node] = {}; // Init dicts
    properties[node].predecessors = G.predecessors(node).length;
    properties[node].successors = G.successors(node).length;
    properties[node].neighbours =
        G.predecessors(node).length + G.successors(node).length;
}
// Shortest paths
const shortestPaths = Object.fromEntries(jsnx.shortestPath(G));
for (const [key, value] of Object.entries(shortestPaths)) {
    const paths = Object.fromEntries(value);
    for (const [k, v] of Object.entries(paths)) {
        paths[k] = v.length - 1;
    }
    for (const node of G.nodes()) {
        if (paths[node]) {
            properties[key][`p_${node}`] = paths[node];
        }
        else {
            properties[key][`p_${node}`] = 0;
        }
    }
}
// Betweeness centrality
const centrality = Object.fromEntries(jsnx.betweennessCentrality(G, { weight: 'b' }));
for (const [key, value] of Object.entries(centrality)) {
    properties[key].betweennessCentrality = value;
}
// Eigenvector centrality
const G_undir = jsnx.convertToUndirected(G);
const g_undir_eigen = Object.fromEntries(jsnx.eigenvectorCentrality(G_undir, { weight: 'b' }));
for (const [key, value] of Object.entries(g_undir_eigen)) {
    properties[key].eigenvectorCentrality = value;
}
// Save results
(async () => {
    const csv = new objects_to_csv_1.default(Object.entries(properties));
    // Save to file:
    await csv.toDisk('networkProperties.csv');
    // Return the CSV file as string:
    console.log(await csv.toString());
})();
debugger;
//# sourceMappingURL=analysis.js.map