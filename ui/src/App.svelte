<script>
	import { onMount } from 'svelte';
	import Login from './Login.svelte';
	import IntegrationList from './IntegrationList.svelte';


	onMount(() => {
	});

</script>

<main class="flex container xl ">
    <h1>Hello!</h1>
    <IntegrationList />

</main>

<style>
    @tailwind base;
    @tailwind components;
    @tailwind utilities;
</style>
