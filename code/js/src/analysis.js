"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
/* eslint-disable @typescript-eslint/ban-types */
const index_1 = require("./index");
const trait_estimates_json_1 = __importDefault(require("./trait estimates.json"));
const trait_extra_info_1 = require("./trait extra info");
const lodash_cloneDeep_1 = __importDefault(require("lodash.cloneDeep"));
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
    trait_estimates_json_1.default.nodes.forEach((e) => {
        if (isInGame(e['label'])) {
            // Exclude happiness since it wasn't in game / test
            output.push({
                id: e['label'],
                delta: trait_extra_info_1.nodeValues[e['id']].units === 'Odds (%)'
                    ? trait_extra_info_1.nodeValues[e['id']].prevalence * 0.33
                    : 1,
                valence: trait_extra_info_1.isGood[e['id']],
            });
        }
    });
    return output;
};
/*

TO DO

Sort out by best effects */
const results = index_1.simulateEverything(edges(), nodes(), true, true, 'noLoopRemoval');
const t = (id, criteria) => {
    const sum = results.sorted.bySumOfEffects.filter((x) => x.origin === id)[0]
        .sumOfEffects;
    const goodness = results.sorted.byBestEffects.filter((x) => x.origin === id)[0].sumOfEffects;
    let effectDepression = results.sorted.byEffectOnNodes
        .filter((x) => x.node === 'Depression')[0]
        .ranks.filter((x) => x.origin === id)[0].results.Depression;
    effectDepression === undefined ? (effectDepression = 0) : null;
    let effectEveningness = results.sorted.byEffectOnNodes
        .filter((x) => x.node === 'Eveningness')[0]
        .ranks.filter((x) => x.origin === id)[0].results.Eveningness;
    effectEveningness === undefined ? (effectEveningness = 0) : null;
    const answers = [
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
            console.log('ERROR: Failed test', `${id}, #${i} (${answers[i]} ! ${q} (${t}) ${a})`);
            return false;
        }
    }
    return true;
};
const tests = {
    sameNNodes: nodes().length === 17,
    sameNEdges: edges().length === 42,
    correctTotalPossibleInterventions: results.unsorted.length === 17,
    highestRankingIsDirect: results.sorted.byEffectOnNodes[0].ranks[0].origin === 'Depression',
    lowerRankingsAreLower: results.sorted.byEffectOnNodes[6].ranks[1].results.Alcohol >=
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
        ['>', 1],
        ['<', 0],
        ['=', 1],
        ['<', 0],
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
const convert = (interventions) => {
    const is = lodash_cloneDeep_1.default(interventions);
    for (i of is) {
        i.steps;
    }
    const o = Object.keys(is)
        .map(function (k) {
        return is[k];
    })
        .join(',');
    return o;
};
const csv = convert(results.unsorted);
/*
fs.writeFile('data.csv', csv, () => {
    console.log('done');
});
*/
debugger;
//# sourceMappingURL=analysis.js.map