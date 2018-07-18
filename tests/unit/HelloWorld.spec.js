import Vue from 'vue'
import Vuex from 'vuex';
import { expect } from 'chai'
import { shallowMount } from '@vue/test-utils'

import store from '@/store.js'
import Settings from '@/components/Settings.vue'

Vue.use(Vuex);

describe('Settings.vue', () => {
    it('renders ok', () => {
        const wrapper = shallowMount(Settings, { store });
        expect(wrapper.text()).to.include('60px');
    })
})
