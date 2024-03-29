import $ from 'jquery';
window.jQuery = $;
window.$ = $;

import * as bootstrap from 'bootstrap';
import 'bootstrap/dist/css/bootstrap.css';
window.bootstrap = bootstrap;


import cytoscape from 'cytoscape';
window.cytoscape = cytoscape;

import dagre from 'cytoscape-dagre';
cytoscape.use( dagre );

import * as CodeMirror from 'codemirror';
window.CodeMirror = Object.assign({}, CodeMirror);

import * as javascript from "@codemirror/lang-javascript";
window.CodeMirror.javascript = javascript;

import { keymap } from '@codemirror/view';
import {EditorState, Compartment} from "@codemirror/state"
import { EditorView, ViewUpdate } from "@codemirror/view"

window.CodeMirror.keymap = keymap;
window.CodeMirror.EditorState = EditorState;
window.CodeMirror.Compartment = Compartment;
window.CodeMirror.EditorState = EditorState;
window.CodeMirror.ViewUpdate = ViewUpdate;

import { autocompletion, snippetCompletion} from '@codemirror/autocomplete';
window.CodeMirror.autocompletion = autocompletion;
window.CodeMirror.snippetCompletion = snippetCompletion;

import jsonview from '@pgrabovets/json-view';
window.jsonview = jsonview;
