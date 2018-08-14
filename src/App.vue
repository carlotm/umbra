<template>
<main class="site-main">
    <Shadows/>
    <Workspace />
</main>
</template>

<script>
import Shadows from './components/Shadows.vue'
import Workspace from './components/Workspace.vue'

export default {
    name: 'app',
    components: {
        Shadows,
        Workspace,
    }
}
</script>
