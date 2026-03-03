import React, { useEffect, useRef } from 'react'
import {
  Box,
  VStack,
  Text,
  HStack,
} from '@chakra-ui/react'

export interface RelationOption {
  label: string
  value: string
  description: string
  color: string
}

export const RELATION_OPTIONS: RelationOption[] = [
  { label: 'Reference',  value: 'reference',  description: 'A cites or links to B',       color: '#4a5568' },
  { label: 'Parent → Child', value: 'parent', description: 'A contains or owns B',         color: '#2b6cb0' },
  { label: 'Related',    value: 'related',    description: 'A and B are loosely related',   color: '#276749' },
  { label: 'Conflicts',  value: 'conflicts',  description: 'A contradicts or blocks B',     color: '#c53030' },
  { label: 'Plain',      value: '',           description: 'No semantic label',             color: '#a0aec0' },
]

interface EdgeRelationMenuProps {
  x: number
  y: number
  onSelect: (relation: RelationOption) => void
  onDismiss: () => void
}

export const EdgeRelationMenu = ({ x, y, onSelect, onDismiss }: EdgeRelationMenuProps) => {
  const ref = useRef<HTMLDivElement>(null)

  // Dismiss on outside click
  useEffect(() => {
    const handler = (e: MouseEvent) => {
      if (ref.current && !ref.current.contains(e.target as Node)) {
        onDismiss()
      }
    }
    document.addEventListener('mousedown', handler)
    return () => document.removeEventListener('mousedown', handler)
  }, [onDismiss])

  // Dismiss on Escape
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onDismiss()
    }
    document.addEventListener('keydown', handler)
    return () => document.removeEventListener('keydown', handler)
  }, [onDismiss])

  // Keep menu inside viewport
  const menuW = 200
  const menuH = RELATION_OPTIONS.length * 48 + 32
  const clampedX = Math.min(x, window.innerWidth - menuW - 8)
  const clampedY = Math.min(y, window.innerHeight - menuH - 8)

  return (
    <Box
      ref={ref}
      position="fixed"
      left={clampedX}
      top={clampedY}
      zIndex={1000}
      bg="white"
      borderRadius="lg"
      boxShadow="lg"
      borderWidth={1}
      borderColor="gray.200"
      w={`${menuW}px`}
      overflow="hidden"
      // Prevent ReactFlow from capturing drag events inside the menu
      className="nodrag nopan"
    >
      <Box px={3} py={2} borderBottomWidth={1} borderColor="gray.100">
        <Text fontSize="xs" fontWeight="600" color="gray.500" textTransform="uppercase" letterSpacing="wide">
          Connection type
        </Text>
      </Box>
      <VStack spacing={0} alignItems="stretch">
        {RELATION_OPTIONS.map((opt) => (
          <Box
            key={opt.value || '__plain__'}
            px={3}
            py={2}
            cursor="pointer"
            _hover={{ bg: 'gray.50' }}
            onClick={() => onSelect(opt)}
          >
            <HStack spacing={2}>
              <Box w="8px" h="8px" borderRadius="full" bg={opt.color} flexShrink={0} />
              <Box>
                <Text fontSize="xs" fontWeight="600" color="gray.700">{opt.label}</Text>
                <Text fontSize="2xs" color="gray.400">{opt.description}</Text>
              </Box>
            </HStack>
          </Box>
        ))}
      </VStack>
    </Box>
  )
}
