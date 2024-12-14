'use client';

import React, {useState, useEffect} from 'react';
import Header from '../../components/Header';
import ThreadCard from '../../components/ThreadCard';
import CommunityList from '../../components/CommunityList';
import CreateCommentModal from '../../components/CreateCommentModal';
import CommentCard from '@/app/components/CommentCard';

interface Comment {
    mainComment: {
        commentIDForCommentInfo: number;
        commentContentForCommentInfo: string;
        createdAtForCommentInfo: string;
        userNameForCommentInfo: string;
    };
    children: Comment[];
}

interface Community {
    communityID: number;
    communityName: string;
}

const ViewThread: React.FC<{params: {id: number}}> = ({params}) => {
    const thread_id = params.id;
    const [thread, setThread] = useState<any>(null);
    const [comments, setComments] = useState<Comment[]>([]);
    const [commentIdForReply, setCommentIdForReply] = useState<number | null>(null);
    const [isLoggedIn, setIsLoggedIn] = useState(false);
    const [communities, setCommunities] = useState<Community[]>([]);
    const [isModalOpen, setIsModalOpen] = useState(false);
    const [newCommentAdded, setNewCommentAdded] = useState(false);
    const [postReaction, setPostReaction] = useState(0);

    const handleCommentVote = async (isUpvote: boolean, commentId: number) => {
        const token = localStorage.getItem('jwt_token');
        if (!token) {
            console.log('User is not authenticated');
            return;
        }
        try {
            const response = await fetch(`http://localhost:8085/api/v1/user/comment/vote/${commentId}/${isUpvote}`, {
                method: 'PUT',
                headers: {
                    'Content-Type': 'application/json',
                    Authorization: `Bearer ${token}`,
                },
            });
        } catch (error) {
            console.log('An error occurred: ' + error);
        }
    };

    const getPostReaction = async () => {
        const token = localStorage.getItem('jwt_token');
        if (token) {
            const response = await fetch('http://localhost:8085/api/v1/user/thread_votes', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    Authorization: `Bearer ${token}`,
                },
                body: JSON.stringify({
                    threadListForVotes: [new Number(thread_id)],
                }),
            });
            const userReactionOnPost = await response.json();
            if (userReactionOnPost.length > 0)
                return (userReactionOnPost[0][1]) ? 1 : 2;
        }
        return 0;
    }

    useEffect(() => {
        const fetchThreadData = async () => {
            try {
                if (thread_id) {
                    const userReactionOnPost = await getPostReaction();
                    setPostReaction(userReactionOnPost);

                    const threadRes = await fetch(`http://localhost:8085/api/v1/thread/${thread_id}`);
                    const threadData = await threadRes.json();
                    setThread(threadData);

                    const commentsRes = await fetch(`http://localhost:8085/api/v1/thread/comment/${thread_id}`);
                    const commentsData = await commentsRes.json();
                    setComments(commentsData.comments);

                    const communitiesRes = await fetch('http://localhost:8085/api/v1/community');
                    const communitiesData = await communitiesRes.json();
                    setCommunities(communitiesData.communities);
                }
            } catch (error) {console.error("Error fetching data:", error);}
        };
        fetchThreadData();
    }, [thread_id]);

    const renderComments = (comments: Comment[]) => {
        return comments.map((comment) => (
            <div key={comment.mainComment.commentIDForCommentInfo} className="ml-4 border-l-2 border-gray-300 pl-4">
                <CommentCard
                    mainComment={comment.mainComment}
                    setIsModalOpen={setIsModalOpen}
                    setCommentIdForReply={setCommentIdForReply}
                    handleCommentVote={handleCommentVote}
                />
                {comment.children.length > 0 && renderComments(comment.children)}
            </div>
        ));
    };

    return (
        <div>
            <Header setIsLoggedIn={setIsLoggedIn} />
            <main className="container mx-auto p-4 flex">
                <div className="w-3/4">
                    {thread && (
                        <ThreadCard
                            key={thread.threadIDForThreadInfo}
                            title={thread.title}
                            communityName={thread.communityNameForThreadInfo}
                            userName={thread.userNameForThreadInfo}
                            createdAt={thread.createdAtForThreadInfo}
                            description={thread.description}
                            upvoteCount={thread.upvoteCount}
                            downvoteCount={thread.downvoteCount}
                            commentCount={thread.commentCount}
                            threadIDForThreadInfo={thread.threadIDForThreadInfo}
                            userPostReaction={postReaction}
                        />
                    )}

                    <div className="mt-6">
                        <button
                            className={`mt-4 px-4 py-2 rounded bg-green-500 text-white ${!isLoggedIn ? 'opacity-50 cursor-not-allowed' : 'hover:bg-green-600'}`}
                            disabled={!isLoggedIn}
                            onClick={() => {setCommentIdForReply(null); setIsModalOpen(true)}}
                        >
                            Add Comment
                        </button>
                        <h2 className="text-2xl font-bold mb-4 text-gray-900">Comments</h2>
                        {renderComments(comments)}
                    </div>
                </div>
                <aside className="w-1/4 pl-4">
                    <CommunityList communities={communities} />
                </aside>
            </main>
            {isModalOpen && (
                <CreateCommentModal
                    closeModal={() => setIsModalOpen(false)}
                    setNewCommentAdded={setNewCommentAdded}
                    threadId={thread_id}
                    replyCommentId={commentIdForReply}
                />
            )}
        </div>
    );
};

export default ViewThread;
