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
