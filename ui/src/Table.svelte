<script lang="ts">

  export let fields = [];
  export let data = [];
  export let key = "id";

</script>
<div class="table bg-white text-left text-sm overflow-x-auto">

  <div class="table-header-group bg-gray-50">
      <div class="table-row">
        {#each fields as field}

        <div class="table-cell px-6 py-2 font-medium text-gray-900">{field.title}</div>
        {/each}
      </div>
  </div>
<div class="table-row-group">

{#each data as d }
  <div class="table-row hover:bg-gray-50" >
    {#each fields as field }
      <div class="table-cell">{d[field.name]}</div>
    {/each}
  </div>
{/each}
</div>

</div>
