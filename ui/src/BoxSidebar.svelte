<script lang="ts">
  import Button from './Button.svelte';

  let show_sidebar = false;
</script>


<div class="w-full md:w-1/3 lg:w-1/4 px-2">

{#if show_sidebar}
  <div class=""><slot></slot></div>
{:else}
  <div class="hidden md:block"><slot></slot></div>
{/if}

  <div class="block md:hidden">
    <Button on:click={() => show_sidebar = !show_sidebar} >
      {#if show_sidebar}
      Hide
      {:else}
      Show
      {/if}
    </Button>
  </div>
</div>
