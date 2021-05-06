import { type } from 'node:os';
// Nodes
export type Node = {
    id: string;
    label?: string; // Trait name
    value?: number; // Prevalence
    delta?: number; // Change due to intervention with this node as its origin
    valence?: 'Good' | 'Neutral' | 'Bad'; // Used in calculating whether changes in this trait are bad or good
};
export type JsnxNode = [Node['id'], Node?];
// Edges
export type Edge = {
    source: Node['id'];
    target: Node['id'];
    beta?: number; // Weight
};
export type JsnxEdge = [Node['id'], Node['id'], Edge?];
// Propagation
export type Step = {
    source: Node['id'];
    deltaX: number;
    target: Node['id'];
    beta: number;
    deltaY: number;
};
export type Intervention = {
    origin: Node['id'];
    steps: Step[];
    results: Record<Node['id'], number>;
    sumOfEffects?: number;
};
export type InterventionSummary = {
    ORIGIN: Node['id'];
    SCORE: number;
    STEPS: number;
    Depression: number | undefined;
    Worry: number | undefined;
    Wellbeing: number | undefined;
    Loneliness: number | undefined;
    Sleeplessness: number | undefined;
    Neuroticism: number | undefined;
    Alcohol: number | undefined;
    Education: number | undefined;
    BMI: number | undefined;
    Intelligence: number | undefined;
    Eveningness: number | undefined;
    'Not socialising': number | undefined;
    Smoking: number | undefined;
    Exercise: number | undefined;
    'Coffee intake': number | undefined;
    CHD: number | undefined;
    Diabetes: number | undefined;
};
