import Vue from 'vue'
import Vuex from 'vuex'
import guybrush from './guybrush.js';

Vue.use(Vuex)

let SHADOWS = [
    {pk: 1, hoff: 5,  voff: 5,   blur: 0, spread: 0, color: '#ff0000'},
    {pk: 2, hoff: 10, voff: 10,  blur: 0,  spread: 0, color: '#00ff00'},
];

export default new Vuex.Store({
    state: {
        seq: SHADOWS.length + 1,
        settings: {
            shape: 'square',
            size: '60',
            color: '#000000',
        },
        shadows: SHADOWS,
        current: SHADOWS[0],
    },
    getters:  {
        currentShadow(state) {
            return state.current;
        },
        shadows(state) {
            return state.shadows;
        },
        cssString(state) {
            let shadowRule = state.shadows.map(s => `${s.hoff}px ${s.voff}px ${s.blur}px ${s.spread}px ${s.color}`).join();
            let shapeRule = state.settings.shape === 'square' ? '0' : '100%';
            let cssStyle = [
                `box-shadow: ${shadowRule}`,
                `border-radius: ${shapeRule}`,
                `width: ${state.settings.size}px`,
                `height: ${state.settings.size}px`,
                `background-color: ${state.settings.color}`,
            ].join(';');
            return cssStyle;
        },
    },
    mutations: {
        ADDSHADOW(state) {
            state.shadows.push({
                pk: state.seq,
                hoff: 10,
                voff: 10,
                blur: 20,
                spread: 2,
                color: '#ff00ff',
            });
            state.seq += 1;
        },
        SETSHAPE(state, value) {
            state.settings.shape = value;
        },
        SETSIZE(state, value) {
            state.settings.size = value;
        },
        SETCOLOR(state, value) {
            state.settings.color = value;
        },
        SETCURRENTSHADOW(state, pk) {
            state.current = state.shadows.find(x => x.pk === pk);
        },
        REMOVECURRENTSHADOW(state) {
            let index = state.shadows.findIndex(x => x.pk === state.current.pk);
            state.current = null;
            state.shadows.splice(index, 1);
        },
        SETHOFF(state, value) {
            state.current.hoff = value;
        },
        SETVOFF(state, value) {
            state.current.voff = value;
        },
        SETBLUR(state, value) {
            state.current.blur = value;
        },
        SETSPREAD(state, value) {
            state.current.spread = value;
        },
        SETCURCOLOR(state, value) {
            state.current.color = value;
        },
        LOADEXAMPLE(state) {
            state.settings = guybrush.settings;
            state.shadows = guybrush.list;
            state.current = guybrush.list[0];
            state.seq = guybrush.list[guybrush.list.length - 1].pk + 1;
        },
        LOAD(state, files) {
            for (let i = 0; i<files.length; i++) {
                let f = files[i];
                // Only process json files.
                if (!f.type.match('.+.json')) {
                    continue;
                }

                let reader = new FileReader();
                reader.onload = (function() {
                    return e => {
                        try {
                            const umbra = JSON.parse(e.target.result);
                            state.settings = umbra.settings;
                            state.shadows = umbra.list;
                            state.current = umbra.list[0];
                            state.seq = umbra.list[umbra.list.length - 1].pk + 1;
                        } catch(e) {
                            alert(e);
                        }
                    };
                })(f);
                reader.readAsText(f);
            }
        },
        SAVE(state) {
            const f = {
                settings: state.settings,
                list: state.shadows,
            };
            const umbra = JSON.stringify(f);
            const encoding = 'data:text/json;charset=utf-8,';
            const data = encoding + encodeURIComponent(umbra);
            const a = document.createElement('a');
            a.setAttribute('href', data);
            a.setAttribute('download', 'umbra.json');
            a.click();
        },
    },
    actions: {
        addShadow(context) {
            context.commit('ADDSHADOW');
        },
        setShape(context, value) {
            context.commit('SETSHAPE', value);
        },
        setSize(context, value) {
            context.commit('SETSIZE', value);
        },
        setColor(context, value) {
            context.commit('SETCOLOR', value);
        },
        setCurrentShadow(context, pk) {
            context.commit('SETCURRENTSHADOW', pk);
        },
        removeCurrentShadow(context) {
            context.commit('REMOVECURRENTSHADOW');
        },
        setHoff(context, value) {
            context.commit('SETHOFF', value);
        },
        setVoff(context, value) {
            context.commit('SETVOFF', value);
        },
        setBlur(context, value) {
            context.commit('SETBLUR', value);
        },
        setSpread(context, value) {
            context.commit('SETSPREAD', value);
        },
        setCurColor(context, value) {
            context.commit('SETCURCOLOR', value);
        },
        loadExample(context) {
            context.commit('LOADEXAMPLE');
        },
        load(context, payload) {
            context.commit('LOAD', payload);
        },
        save(context) {
            context.commit('SAVE');
        },
        export(context) {
            alert(context.getters.cssString);
        },
    }
})
