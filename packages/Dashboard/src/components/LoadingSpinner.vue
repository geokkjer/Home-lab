<template>
  <div class="flex items-center justify-center space-x-2">
    <svg class="animate-spin h-5 w-5 text-blue-500" viewBox="0 0 24 24">
      <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
      <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
    </svg>
    <span class="text-gray-500">Loading...</span>
  </div>
</template>

<script setup lang="ts">
// No props needed for this simple loading spinner
</script>
