import React, { useState, useRef, useEffect } from 'react'
import {
  Flex,
  Button,
  Select,
  Input,
  IconButton,
  HStack,
  Box,
  Tooltip,
  useDisclosure,
  Modal,
  ModalOverlay,
  ModalContent,
  ModalHeader,
  ModalBody,
  ModalFooter,
  ModalCloseButton,
  AlertDialog,
  AlertDialogBody,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogContent,
  AlertDialogOverlay,
  InputGroup,
  InputLeftElement,
  InputRightElement,
  Text,
} from '@chakra-ui/react'
import { AddIcon, DeleteIcon, SearchIcon, CloseIcon, RepeatIcon } from '@chakra-ui/icons'
import { useBoardStore } from '../store/boardStore'
import { useBoardContext } from '../store/BoardContext'
import { ConnectionState } from '../hooks/useWebSocket'

interface ToolbarProps {
  sendCommand: (command: string, data?: any) => void
  connectionState: ConnectionState
  sendFailed: boolean
  showPalette?: boolean
  onTogglePalette?: () => void
}

const connectionColors: Record<ConnectionState, string> = {
  connecting: 'yellow.400',
  connected: 'green.400',
  disconnected: 'red.400',
}

const connectionLabels: Record<ConnectionState, string> = {
  connecting: 'Connecting...',
  connected: 'Connected',
  disconnected: 'Disconnected',
}

export const Toolbar = ({ sendCommand, connectionState, sendFailed, showPalette, onTogglePalette }: ToolbarProps) => {
  const { boards, currentBoardId, boardTitle, searchQuery, setSearchQuery, nodes } = useBoardStore()
  const { onAutoLayout } = useBoardContext()
  const { isOpen, onOpen, onClose } = useDisclosure()
  const {
    isOpen: isDeleteOpen,
    onOpen: onDeleteOpen,
    onClose: onDeleteClose,
  } = useDisclosure()
  const [newTitle, setNewTitle] = useState('')
  const cancelRef = useRef<HTMLButtonElement>(null)
  const searchInputRef = useRef<HTMLInputElement>(null)

  // P2: Global keyboard shortcut for search (Ctrl+F / Cmd+F)
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && e.key === 'f') {
        e.preventDefault()
        searchInputRef.current?.focus()
        searchInputRef.current?.select()
      }
    }
    window.addEventListener('keydown', handler)
    return () => window.removeEventListener('keydown', handler)
  }, [])

  // P2: Compute match count for the indicator
  const matchCount = searchQuery.trim()
    ? nodes.filter((n) => {
        const q = searchQuery.toLowerCase()
        return (
          n.title.toLowerCase().includes(q) ||
          n.content?.toLowerCase().includes(q) ||
          n.tags.some((t) => t.toLowerCase().includes(q))
        )
      }).length
    : null

  const handleBoardChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const boardId = e.target.value
    if (boardId) {
      sendCommand('open-board', { boardId })
    }
  }

  const handleCreateBoard = () => {
    if (newTitle.trim()) {
      sendCommand('create-board', { title: newTitle.trim() })
      setNewTitle('')
      onClose()
    }
  }

  const handleDeleteBoard = () => {
    if (currentBoardId) {
      sendCommand('delete-board', { boardId: currentBoardId })
      onDeleteClose()
    }
  }

  return (
    <>
      <Flex
        position="absolute"
        top={0}
        left={0}
        right={0}
        h="48px"
        bg="white"
        borderBottomWidth={1}
        borderColor="gray.200"
        px={4}
        alignItems="center"
        zIndex={10}
        gap={3}
      >
        <Select
          size="sm"
          maxW="250px"
          value={currentBoardId || ''}
          onChange={handleBoardChange}
          placeholder="Select a board..."
        >
          {boards.map((b) => (
            <option key={b.id} value={b.id}>
              {b.title}
            </option>
          ))}
        </Select>

        <HStack spacing={1}>
          <IconButton
            aria-label="New board"
            icon={<AddIcon />}
            size="sm"
            variant="ghost"
            onClick={onOpen}
          />
          <IconButton
            aria-label="Delete board"
            icon={<DeleteIcon />}
            size="sm"
            variant="ghost"
            colorScheme="red"
            onClick={onDeleteOpen}
            isDisabled={!currentBoardId}
          />
        </HStack>

        {currentBoardId && onTogglePalette && (
          <Button
            size="sm"
            variant={showPalette ? 'solid' : 'outline'}
            colorScheme="blue"
            leftIcon={<AddIcon />}
            onClick={onTogglePalette}
          >
            Add Node
          </Button>
        )}

        {/* P1b: Auto-layout button */}
        {currentBoardId && onAutoLayout && (
          <Tooltip label="Auto-layout nodes" fontSize="xs">
            <IconButton
              aria-label="Auto layout"
              icon={<RepeatIcon />}
              size="sm"
              variant="ghost"
              onClick={onAutoLayout}
            />
          </Tooltip>
        )}

        {/* P2: Search bar */}
        {currentBoardId && (
          <InputGroup size="sm" maxW="220px" ml={2}>
            <InputLeftElement pointerEvents="none">
              <SearchIcon color="gray.400" boxSize={3} />
            </InputLeftElement>
            <Input
              ref={searchInputRef}
              placeholder="Search nodes…"
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              borderRadius="md"
              fontSize="xs"
              pr={searchQuery ? '28px' : undefined}
            />
            {searchQuery && (
              <InputRightElement>
                <IconButton
                  aria-label="Clear search"
                  icon={<CloseIcon boxSize={2} />}
                  size="xs"
                  variant="ghost"
                  onClick={() => setSearchQuery('')}
                />
              </InputRightElement>
            )}
          </InputGroup>
        )}

        {/* P2: Match count indicator */}
        {matchCount !== null && (
          <Text fontSize="xs" color="gray.500" whiteSpace="nowrap">
            {matchCount} / {nodes.length}
          </Text>
        )}

        {boardTitle && (
          <Flex ml="auto" fontSize="sm" color="gray.500">
            {boardTitle}
          </Flex>
        )}

        {/* Connection state indicator */}
        <Tooltip label={connectionLabels[connectionState]} fontSize="xs">
          <Box
            ml={boardTitle ? 2 : 'auto'}
            w="8px"
            h="8px"
            borderRadius="full"
            bg={connectionColors[connectionState]}
            flexShrink={0}
            boxShadow={
              sendFailed
                ? '0 0 0 2px rgba(245, 101, 101, 0.5)'
                : connectionState === 'disconnected'
                ? '0 0 0 2px rgba(245, 101, 101, 0.3)'
                : 'none'
            }
            transition="all 0.3s"
          />
        </Tooltip>
      </Flex>

      {/* Create Board Modal */}
      <Modal isOpen={isOpen} onClose={onClose} size="sm">
        <ModalOverlay />
        <ModalContent>
          <ModalHeader>Create New Board</ModalHeader>
          <ModalCloseButton />
          <ModalBody>
            <Input
              placeholder="Board title"
              value={newTitle}
              onChange={(e) => setNewTitle(e.target.value)}
              onKeyDown={(e) => e.key === 'Enter' && handleCreateBoard()}
              autoFocus
            />
          </ModalBody>
          <ModalFooter>
            <Button size="sm" mr={3} onClick={onClose}>
              Cancel
            </Button>
            <Button size="sm" colorScheme="blue" onClick={handleCreateBoard}>
              Create
            </Button>
          </ModalFooter>
        </ModalContent>
      </Modal>

      {/* Delete Board AlertDialog */}
      <AlertDialog
        isOpen={isDeleteOpen}
        leastDestructiveRef={cancelRef as React.RefObject<any>}
        onClose={onDeleteClose}
      >
        <AlertDialogOverlay>
          <AlertDialogContent>
            <AlertDialogHeader fontSize="lg" fontWeight="bold">
              Delete Board
            </AlertDialogHeader>
            <AlertDialogBody>
              Are you sure you want to delete "{boardTitle}"? This action cannot be undone.
            </AlertDialogBody>
            <AlertDialogFooter>
              <Button ref={cancelRef as React.RefObject<any>} size="sm" onClick={onDeleteClose}>
                Cancel
              </Button>
              <Button size="sm" colorScheme="red" onClick={handleDeleteBoard} ml={3}>
                Delete
              </Button>
            </AlertDialogFooter>
          </AlertDialogContent>
        </AlertDialogOverlay>
      </AlertDialog>
    </>
  )
}
