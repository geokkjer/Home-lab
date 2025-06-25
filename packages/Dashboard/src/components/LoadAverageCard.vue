<template>
  <div class="bg-white overflow-hidden shadow rounded-lg">
    <div class="p-5">
      <div class="flex items-center">
        <div class="flex-shrink-0">
          <svg class="h-6 w-6 text-gray-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
          </svg>
        </div>
        <div class="ml-5 w-0 flex-1">
          <dl>
            <dt class="text-sm font-medium text-gray-500 truncate">Load Average</dt>
            <dd class="text-lg font-medium text-gray-900">{{ load[0].toFixed(2) }}</dd>
          </dl>
        </div>
      </div>
    </div>
    <div class="bg-gray-50 px-5 py-3">
      <div class="grid grid-cols-3 gap-4 text-sm">
        <div class="text-center">
          <span class="block font-medium">{{ load[0].toFixed(2) }}</span>
          <span class="text-gray-500">1m</span>
        </div>
        <div class="text-center">
          <span class="block font-medium">{{ load[1].toFixed(2) }}</span>
          <span class="text-gray-500">5m</span>
        </div>
        <div class="text-center">
          <span class="block font-medium">{{ load[2].toFixed(2) }}</span>
          <span class="text-gray-500">15m</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
interface Props {
  load: readonly number[]
}

defineProps<Props>()
</script>
