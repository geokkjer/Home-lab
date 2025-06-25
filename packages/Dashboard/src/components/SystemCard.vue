<template>
  <div class="bg-white overflow-hidden shadow rounded-lg">
    <div class="p-5">
      <div class="flex items-center">
        <div class="flex-shrink-0">
          <svg class="h-6 w-6 text-gray-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 3v2m6-2v2M9 19v2m6-2v2M5 9H3m2 6H3m18-6h-2m2 6h-2M7 19h10a2 2 0 002-2V7a2 2 0 00-2-2H7a2 2 0 00-2 2v10a2 2 0 002 2zM9 9h6v6H9V9z" />
          </svg>
        </div>
        <div class="ml-5 w-0 flex-1">
          <dl>
            <dt class="text-sm font-medium text-gray-500 truncate">System Information</dt>
            <dd class="text-lg font-medium text-gray-900">{{ systemInfo.hostname }}</dd>
          </dl>
        </div>
      </div>
    </div>
    <div class="bg-gray-50 px-5 py-3">
      <div class="grid grid-cols-2 gap-4 text-sm">
        <div>
          <span class="text-gray-500">Memory:</span>
          <span class="ml-2 font-medium">{{ memoryUsage }}%</span>
        </div>
        <div>
          <span class="text-gray-500">Disk:</span>
          <span class="ml-2 font-medium">{{ diskUsage }}%</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import type { SystemInfo } from '@/stores/dashboard'

interface Props {
  systemInfo: SystemInfo
  memoryUsage: number
  diskUsage: number
}

defineProps<Props>()
</script>
