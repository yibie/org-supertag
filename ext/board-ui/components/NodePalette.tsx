import React, { useState, useEffect, useMemo, useCallback } from 'react'
import {
  Box,
  VStack,
  Input,
  Text,
  Tag,
  HStack,
  Flex,
  IconButton,
  Accordion,
  AccordionItem,
  AccordionButton,
  AccordionPanel,
  AccordionIcon,
  Badge,
} from '@chakra-ui/react'
import { CloseIcon, SearchIcon, DragHandleIcon } from '@chakra-ui/icons'
import { useReactFlow } from '@xyflow/react'
import { useBoardStore } from '../store/boardStore'
import { AvailableNode } from '../store/types'

interface NodePaletteProps {
  isOpen: boolean
  onClose: () => void
  sendCommand: (command: string, data?: any) => void
}

// Data key used for drag-and-drop transfers
export const PALETTE_DND_TYPE = 'application/org-supertag-node-id'

const NodeItem = ({
  node,
  onAdd,
}: {
  node: AvailableNode
  onAdd: (id: string) => void
}) => {
  const handleDragStart = (e: React.DragEvent) => {
    e.dataTransfer.setData(PALETTE_DND_TYPE, node.id)
    e.dataTransfer.effectAllowed = 'copy'
  }

  return (
    <Box
      p={2}
      borderRadius="md"
      cursor="grab"
      draggable
      onDragStart={handleDragStart}
      onClick={() => onAdd(node.id)}
      _hover={{ bg: 'blue.50' }}
      _active={{ cursor: 'grabbing' }}
      display="flex"
      alignItems="flex-start"
      gap={1.5}
    >
      <Box pt="2px" color="gray.300" flexShrink={0}>
        <DragHandleIcon boxSize={2.5} />
      </Box>
      <Box flex={1} minW={0}>
        <Text fontSize="sm" fontWeight="500" noOfLines={1}>
          {node.title}
        </Text>
        {node.tags.length > 1 && (
          <HStack mt={1} spacing={1} flexWrap="wrap">
            {node.tags.slice(1, 4).map((tag) => (
              <Tag key={tag} size="sm" fontSize="2xs" variant="subtle" colorScheme="gray">
                {tag}
              </Tag>
            ))}
          </HStack>
        )}
      </Box>
    </Box>
  )
}

export const NodePalette = ({ isOpen, onClose, sendCommand }: NodePaletteProps) => {
  const { availableNodes, currentBoardId, nodes } = useBoardStore()
  const [search, setSearch] = useState('')

  const reactFlowInstance = useReactFlow()

  useEffect(() => {
    if (isOpen) {
      sendCommand('list-nodes')
    }
  }, [isOpen, sendCommand])

  const placedIds = useMemo(() => new Set(nodes.map((n) => n.id)), [nodes])

  const filtered = useMemo(
    () =>
      availableNodes
        .filter((n) => !placedIds.has(n.id))
        .filter(
          (n) =>
            !search ||
            n.title.toLowerCase().includes(search.toLowerCase()) ||
            n.tags.some((t) => t.toLowerCase().includes(search.toLowerCase()))
        ),
    [availableNodes, placedIds, search]
  )

  const grouped = useMemo(() => {
    const tagMap: Record<string, AvailableNode[]> = {}
    const untagged: AvailableNode[] = []

    for (const node of filtered) {
      if (!node.tags || node.tags.length === 0) {
        untagged.push(node)
      } else {
        const primaryTag = node.tags[0]
        if (!tagMap[primaryTag]) tagMap[primaryTag] = []
        tagMap[primaryTag].push(node)
      }
    }

    const sorted = Object.entries(tagMap).sort((a, b) => {
      if (b[1].length !== a[1].length) return b[1].length - a[1].length
      return a[0].localeCompare(b[0])
    })

    return { tagged: sorted, untagged }
  }, [filtered])

  const handleAddNode = useCallback(
    (nodeId: string) => {
      if (!currentBoardId) return
      const centerScreen = { x: window.innerWidth / 2, y: window.innerHeight / 2 }
      const flowPos = reactFlowInstance.screenToFlowPosition(centerScreen)
      const x = flowPos.x + (Math.random() - 0.5) * 100
      const y = flowPos.y + (Math.random() - 0.5) * 80
      sendCommand('add-node', { boardId: currentBoardId, nodeId, x, y })
    },
    [currentBoardId, sendCommand, reactFlowInstance]
  )

  if (!isOpen) return null

  const allGroups = [
    ...grouped.tagged,
    ...(grouped.untagged.length > 0 ? [['Untagged', grouped.untagged] as const] : []),
  ]

  let emptyMessage = 'No nodes found'
  if (availableNodes.length === 0) {
    emptyMessage = 'No nodes in store (sync your notes first)'
  } else if (!search && filtered.length === 0) {
    emptyMessage = 'All nodes are already on this board'
  } else if (search && filtered.length === 0) {
    emptyMessage = 'No nodes match your search'
  }

  return (
    <Box
      position="absolute"
      top="48px"
      right={0}
      w="320px"
      h="calc(100vh - 48px)"
      bg="white"
      borderLeftWidth={1}
      borderColor="gray.200"
      zIndex={10}
      overflow="hidden"
    >
      <Flex direction="column" h="100%">
        <Flex p={3} alignItems="center" gap={2} borderBottomWidth={1} borderColor="gray.100">
          <SearchIcon color="gray.400" />
          <Input
            size="sm"
            variant="unstyled"
            placeholder="Search nodes..."
            value={search}
            onChange={(e) => setSearch(e.target.value)}
            autoFocus
          />
          <IconButton
            aria-label="Close"
            icon={<CloseIcon />}
            size="xs"
            variant="ghost"
            onClick={onClose}
          />
        </Flex>

        <Flex fontSize="xs" color="gray.400" px={3} pt={2} pb={1} alignItems="center" gap={1}>
          <DragHandleIcon boxSize={2} />
          <Text>Drag to canvas or click to place at center</Text>
        </Flex>

        <Box flex={1} overflowY="auto">
          {search ? (
            <VStack p={2} spacing={0} alignItems="stretch">
              {filtered.map((node) => (
                <NodeItem key={node.id} node={node} onAdd={handleAddNode} />
              ))}
            </VStack>
          ) : (
            <Accordion allowMultiple defaultIndex={[]}>
              {allGroups.map(([tag, tagNodes]) => (
                <AccordionItem key={tag} border="none">
                  <AccordionButton px={3} py={2} _hover={{ bg: 'gray.50' }}>
                    <Flex flex={1} alignItems="center" gap={2}>
                      <Text fontSize="xs" fontWeight="600" color="gray.600">
                        {tag}
                      </Text>
                      <Badge
                        size="sm"
                        variant="subtle"
                        colorScheme="gray"
                        borderRadius="full"
                        fontSize="2xs"
                      >
                        {(tagNodes as AvailableNode[]).length}
                      </Badge>
                    </Flex>
                    <AccordionIcon />
                  </AccordionButton>
                  <AccordionPanel px={1} py={0}>
                    {(tagNodes as AvailableNode[]).map((node) => (
                      <NodeItem key={node.id} node={node} onAdd={handleAddNode} />
                    ))}
                  </AccordionPanel>
                </AccordionItem>
              ))}
            </Accordion>
          )}

          {filtered.length === 0 && (
            <Text fontSize="sm" color="gray.400" textAlign="center" py={4}>
              {emptyMessage}
            </Text>
          )}
        </Box>
      </Flex>
    </Box>
  )
}
