"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const loops_1 = require("../src/loops");
const traversal_1 = require("../src/traversal");
const propagation_1 = require("../src/propagation");
const interventions_1 = require("../src/interventions");
const format_1 = require("../src/format");
const index_1 = require("../src/index");
const testData_1 = require("./testData");
// Use tests and methods
const runTests = (origin, testData, edges, nodes) => {
    const tests = {};
    tests.duplicateEdgesRemoved = testData.edges(true).length === 18;
    const loops = loops_1.identifyAndRemoveLoops(testData, origin);
    tests.detectedLoops = loops.length === 8;
    tests.removedLoops = testData.edges(true).length === 10;
    const path = traversal_1.calculatePropagationPath(testData, origin);
    tests.correctNumSteps = path.length === 10;
    // Calculate intervention
    const propagationResults = propagation_1.propagate(path, origin, 1);
    const checkAnswers = (r, a) => {
        for (const [k, v] of Object.entries(r)) {
            if (!(a[k] === v)) {
                return false;
            }
        }
        return true;
    };
    tests.correctAnswer = checkAnswers(propagationResults.results, {
        A: 1,
        B: 0.5,
        C: 0.75,
        D: 0.375,
        E: 0.46875,
        F: 1.125,
        G: 0.5625,
    });
    // All possible interventions
    const allPossibleInterventions = interventions_1.calculateAllInterventionEffects(testData);
    tests.gotAllPossibleInterventions = allPossibleInterventions.length === 7;
    const checkInterventions = (a) => {
        for (const i of a) {
            if (typeof i.origin != 'string') {
                return false;
            }
            if (!(Object.keys(i.results).length > 0)) {
                return false;
            }
            if (!(i.steps.length >= 0)) {
                return false;
            }
        }
        return true;
    };
    tests.interventionsLookSensible = checkInterventions(allPossibleInterventions);
    // Sorting by effectiveness
    const sortedBySumOfEffects = interventions_1.sortInterventions(allPossibleInterventions, {
        sumOfEffects: true,
    });
    tests.identifiesBestInterventionForOverallSumOfEffects =
        sortedBySumOfEffects[0].origin === 'A';
    const sortedByEffectOnNode = interventions_1.sortInterventions(allPossibleInterventions, {
        effectOnNode: 'D',
    });
    tests.identifiesBestInterventionForSpecificNode =
        sortedByEffectOnNode[1].origin === 'C';
    const sortedByBestEffects = interventions_1.sortInterventions(allPossibleInterventions, {
        bestEffects: testData.nodes(true),
    });
    if (nodes) {
        tests.identifiesInterventionWithBestEffects =
            sortedByBestEffects[0].origin === 'E';
    }
    // Easy functions
    const easyFunc1 = index_1.simulateIntervention(edges, 'A', 1, nodes);
    const easyFunc2 = index_1.simulateEverything(edges, nodes, nodes ? true : false, nodes ? true : false);
    tests.easyFunction1Worked =
        easyFunc1.origin == propagationResults.origin &&
            easyFunc1.results.E == propagationResults.results.E &&
            easyFunc1.steps.length == propagationResults.steps.length;
    tests.easyFunction2Worked =
        easyFunc2.unsorted.length == allPossibleInterventions.length &&
            easyFunc2.unsorted[5].origin == allPossibleInterventions[5].origin &&
            easyFunc2.unsorted[5].steps.length ==
                allPossibleInterventions[5].steps.length &&
            easyFunc2.unsorted[5].results.G ==
                allPossibleInterventions[5].results.G;
    return tests;
};
const testFullData = runTests('A', format_1.formatData(testData_1.exampleEdges, testData_1.exampleNodes), testData_1.exampleEdges, testData_1.exampleNodes);
const testPartialData = runTests('A', format_1.formatData(testData_1.exampleEdges), testData_1.exampleEdges);
const passing = Object.values(testFullData).reduce((a, value) => a && value) === true &&
    Object.values(testPartialData).reduce((a, value) => a && value) === true;
console.log(passing);
debugger;
//# sourceMappingURL=tests.js.map