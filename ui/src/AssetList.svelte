<script lang="ts">

  import { onMount } from 'svelte';
  import Table from './Table.svelte';
  import TableElementTime from './TableElementTime.svelte';
  import Button from './Button.svelte';
  import FilterButton from './FilterButton.svelte';

  export let filters = {price_greater_than : "0"};

  let message = "";
  let fields = [
    {name: "symbol", title: "Symbol"},
    {name: "external_id", title: "ID"},
    {name: "name", title: "Name"},
    {name: "last_price", title: "Price"},
    {name: "last_price_timestamp", title: "Updated", component: TableElementTime},
    ];
  let assets = [];

  $: getAssets(filters);

  function filters_to_params(filters) {
      return filters
  };
  function clearFilter(key) {
    delete filters[key];
    filters = filters;
  }

  async function getAssets(Filters) {
    let params = filters_to_params(filters);
    let path = "/api/assets?" + new URLSearchParams(params);
    let response = await fetch(path, {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        assets = json.assets;
    } else {
        message = json.message;
    };
  }

  onMount(() => {});

</script>

<div class="w-full">

  <div class="">
  Filters:

  {#if filters.price_greater_than}
    <FilterButton on:click={() => clearFilter("price_greater_than")}>Price Greater Than:
    ${ filters.price_greater_than }
  </FilterButton>
  {/if}

  </div>

<Table fields={fields} data={assets} />

</div>
