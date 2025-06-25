<template>
  <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
    <button
      v-for="action in actions"
      :key="action.id"
      type="button"
      :class="[
        'relative flex items-center space-x-3 rounded-lg border border-gray-300 bg-white px-6 py-5 shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2',
        action.enabled 
          ? 'hover:border-gray-400 cursor-pointer' 
          : 'opacity-50 cursor-not-allowed'
      ]"
      :disabled="!action.enabled"
      @click="handleAction(action.id)"
    >
      <div class="flex-shrink-0">
        <component 
          :is="action.icon" 
          :class="[
            'h-6 w-6',
            action.enabled ? action.color : 'text-gray-400'
          ]"
        />
      </div>
      <div class="min-w-0 flex-1">
        <span class="absolute inset-0" aria-hidden="true"></span>
        <p class="text-sm font-medium text-gray-900">{{ action.name }}</p>
        <p class="text-sm text-gray-500 truncate">{{ action.description }}</p>
      </div>
    </button>
  </div>
</template>

<script setup lang="ts">
import { h } from 'vue'

interface Emits {
  (e: 'action', actionId: string): void
}

const emit = defineEmits<Emits>()

// Icon components
const RefreshIcon = () => h('svg', { 
  fill: 'none', 
  viewBox: '0 0 24 24', 
  stroke: 'currentColor' 
}, [
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15'
  })
])

const TerminalIcon = () => h('svg', { 
  fill: 'none', 
  viewBox: '0 0 24 24', 
  stroke: 'currentColor' 
}, [
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M8 9l3 3-3 3m5 0h3M5 20h14a2 2 0 002-2V6a2 2 0 00-2-2H5a2 2 0 00-2 2v14a2 2 0 002 2z'
  })
])

const ChartIcon = () => h('svg', { 
  fill: 'none', 
  viewBox: '0 0 24 24', 
  stroke: 'currentColor' 
}, [
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z'
  })
])

const SettingsIcon = () => h('svg', { 
  fill: 'none', 
  viewBox: '0 0 24 24', 
  stroke: 'currentColor' 
}, [
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z'
  }),
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M15 12a3 3 0 11-6 0 3 3 0 016 0z'
  })
])

const actions = [
  {
    id: 'refresh',
    name: 'Refresh All',
    description: 'Update all service status',
    icon: RefreshIcon,
    color: 'text-blue-600',
    enabled: true
  },
  {
    id: 'netdata',
    name: 'Open Netdata',
    description: 'View detailed metrics',
    icon: ChartIcon,
    color: 'text-green-600',
    enabled: true
  },
  {
    id: 'terminal',
    name: 'SSH Access',
    description: 'Connect via terminal',
    icon: TerminalIcon,
    color: 'text-gray-600',
    enabled: false
  },
  {
    id: 'settings',
    name: 'System Settings',
    description: 'Configure services',
    icon: SettingsIcon,
    color: 'text-purple-600',
    enabled: false
  }
]

const handleAction = (actionId: string) => {
  const action = actions.find(a => a.id === actionId)
  if (action?.enabled) {
    emit('action', actionId)
  }
}
</script>
