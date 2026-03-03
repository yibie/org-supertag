import React, { memo, useState } from 'react'
import { Handle, Position, NodeProps } from '@xyflow/react'
import { Box, Text, Tag, HStack, Flex, Badge, VStack } from '@chakra-ui/react'

interface BoardNodeData {
  title: string
  tags: string[]
  tagFields: Record<string, Array<{ name: string; value: string }>>
  content: string
  collapsed: boolean
  isFollowed: boolean
  onDoubleClick: (nodeId: string) => void
  onDelete: (nodeId: string) => void
}

const BoardNodeComponent = ({ id, data }: NodeProps) => {
  const { title, tags, tagFields, content, collapsed, isFollowed, onDoubleClick, onDelete } =
    data as unknown as BoardNodeData
  const maxVisibleTags = 3
  const [expandedTags, setExpandedTags] = useState<Record<string, boolean>>({})
  const [contentExpanded, setContentExpanded] = useState(false)
  const [isHovered, setIsHovered] = useState(false)

  const toggleTag = (tag: string) => {
    setExpandedTags((prev) => ({ ...prev, [tag]: !prev[tag] }))
  }

  // Bar-shaped handles: only visible on node hover.
  // The exact handle that's grabbed doesn't matter — ConnectionLine always
  // starts from the card boundary toward the cursor (via getNodeIntersection).
  const hBar: React.CSSProperties = {
    width: 48,
    height: 8,
    background: 'rgba(74, 144, 217, 0.35)',
    border: '1px solid rgba(74,144,217,0.7)',
    borderRadius: 4,
    cursor: 'crosshair',
    transition: 'opacity 0.15s',
    zIndex: 10,
    opacity: isHovered ? 1 : 0,
    pointerEvents: isHovered ? 'all' : 'none',
  }
  const vBar: React.CSSProperties = {
    width: 8,
    height: 32,
    background: 'rgba(74, 144, 217, 0.35)',
    border: '1px solid rgba(74,144,217,0.7)',
    borderRadius: 4,
    cursor: 'crosshair',
    transition: 'opacity 0.15s',
    zIndex: 10,
    opacity: isHovered ? 1 : 0,
    pointerEvents: isHovered ? 'all' : 'none',
  }

  return (
    <Box
      onMouseEnter={() => setIsHovered(true)}
      onMouseLeave={() => setIsHovered(false)}
      position="relative"
    >
      <Handle type="source" position={Position.Top}    style={hBar} />
      <Handle type="source" position={Position.Bottom} style={hBar} />
      <Handle type="source" position={Position.Left}   style={vBar} />
      <Handle type="source" position={Position.Right}  style={vBar} />

      <Box
        role="group"
        bg="white"
        borderWidth={isFollowed ? 2 : 1}
        borderColor={isFollowed ? 'blue.400' : 'gray.200'}
        borderRadius="md"
        boxShadow={isFollowed ? 'md' : 'sm'}
        w="180px"
        cursor="grab"
        onDoubleClick={() => onDoubleClick(id)}
        _hover={{ boxShadow: 'md', borderColor: 'blue.200' }}
        transition="all 0.1s"
        position="relative"
      >
        <Box
          as="button"
          type="button"
          aria-label="Delete card"
          title="Delete card"
          onClick={(e) => {
            e.stopPropagation()
            onDelete(id)
          }}
          position="absolute"
          top="4px"
          right="4px"
          w="18px"
          h="18px"
          border="none"
          borderRadius="full"
          bg="red.400"
          color="white"
          fontSize="12px"
          lineHeight="18px"
          textAlign="center"
          opacity={0}
          _groupHover={{ opacity: 1 }}
          _hover={{ bg: 'red.500' }}
          transition="opacity 0.12s"
          zIndex={2}
        >
          ×
        </Box>
        <Flex direction="column" p={2} gap={1}>
          <Text fontSize="xs" fontWeight="600" noOfLines={collapsed ? 1 : 2} color="gray.800">
            {title || 'Untitled'}
          </Text>
          {!collapsed && (
            <Text
              as="button"
              type="button"
              fontSize="2xs"
              color="blue.500"
              textAlign="left"
              onClick={(e) => {
                e.stopPropagation()
                setContentExpanded((prev) => !prev)
              }}
            >
              {contentExpanded ? '收起内容' : '展开内容'}
            </Text>
          )}
          {!collapsed && contentExpanded && (
            <Box
              maxH="140px"
              overflowY="auto"
              bg="gray.50"
              borderRadius="sm"
              borderWidth="1px"
              borderColor="gray.200"
              p={2}
            >
              <Text fontSize="2xs" color={content ? 'gray.700' : 'gray.400'} whiteSpace="pre-wrap">
                {content || '(empty)'}
              </Text>
            </Box>
          )}

          {tags && tags.length > 0 && !collapsed && (
            <VStack spacing={1} align="stretch">
              <HStack spacing={1} flexWrap="wrap">
                {tags.slice(0, maxVisibleTags).map((tag) => (
                  <Tag
                    key={tag}
                    as="button"
                    size="sm"
                    fontSize="2xs"
                    colorScheme={expandedTags[tag] ? 'blue' : 'gray'}
                    variant="subtle"
                    onClick={(e) => {
                      e.stopPropagation()
                      toggleTag(tag)
                    }}
                  >
                    {tag}
                  </Tag>
                ))}
              </HStack>
              {tags.slice(0, maxVisibleTags).map((tag) => {
                if (!expandedTags[tag]) return null
                const fields = tagFields?.[tag] || []
                return (
                  <Box key={`${tag}-fields`} bg="gray.50" borderRadius="sm" px={2} py={1}>
                    <Text fontSize="2xs" fontWeight="600" color="gray.700" mb={0.5}>
                      {tag}
                    </Text>
                    {fields.length === 0 ? (
                      <Text fontSize="2xs" color="gray.400">
                        (no fields)
                      </Text>
                    ) : (
                      fields.map((field) => (
                        <Text key={`${tag}-${field.name}`} fontSize="2xs" color="gray.600">
                          <Text as="span" fontWeight="600">
                            {field.name}:
                          </Text>{' '}
                          <Text as="span" color={field.value ? 'gray.600' : 'gray.400'}>
                            {field.value || '(empty)'}
                          </Text>
                        </Text>
                      ))
                    )}
                  </Box>
                )
              })}
              {tags.length > maxVisibleTags && (
                <Badge
                  fontSize="2xs"
                  colorScheme="gray"
                  variant="subtle"
                  borderRadius="full"
                  px={1.5}
                >
                  +{tags.length - maxVisibleTags}
                </Badge>
              )}
            </VStack>
          )}
        </Flex>
      </Box>
    </Box>
  )
}

export default memo(BoardNodeComponent)
