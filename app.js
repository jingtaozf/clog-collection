import $ from 'jquery';
window.jQuery = $;
window.$ = $;

import cytoscape from 'cytoscape';
window.cytoscape = cytoscape;

import dagre from 'cytoscape-dagre';
cytoscape.use( dagre );

import * as codemirror from 'codemirror';
window.codemirror = codemirror;
