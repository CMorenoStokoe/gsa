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
Object.defineProperty(exports, "__esModule", { value: true });
exports.formatData = void 0;
const jsnx = __importStar(require("jsnetworkx"));
const formatData = (edges, nodes) => {
    const G = new jsnx.DiGraph();
    if (nodes) {
        const arrayOfNodes = nodes.map((x) => [x.id, x]);
        G.addNodesFrom(arrayOfNodes);
    }
    const arrayOfEdges = edges.map((x) => [x.source, x.target, x]);
    G.addEdgesFrom(arrayOfEdges);
    return G;
};
exports.formatData = formatData;
//# sourceMappingURL=format.js.map